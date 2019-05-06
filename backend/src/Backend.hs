{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Backend where

import Control.Lens.Operators ((<&>))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Sum
import Data.List (isSuffixOf)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Natural (Natural)
import System.Directory
import System.FilePath ((</>))

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite (Sqlite, runBeamSqlite)
import qualified Database.SQLite.Simple as SQLite
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Data.Pagination
import Snap
import Snap.Snaplet.Session
import Web.ClientSession

import Obelisk.Backend as Ob
import qualified Obelisk.ExecutableConfig as Cfg
import Obelisk.OAuth.AccessToken
import Obelisk.OAuth.Authorization
import Obelisk.Route hiding (decode, encode)

import Common.Route
import Common.Slack.Types
import Common.Slack.Types.Auth

data Query
  = Query_Day Day
  | Query_Like Text

filterQuery
  :: forall be s.
     ( BeamSqlBackend be
     , BeamSqlBackendIsString be Text
     , HasSqlValueSyntax (BeamSqlBackendValueSyntax be) Text
     , HasSqlValueSyntax (BeamSqlBackendValueSyntax be) UTCTime
     )
  => Query
  -> Q be SlackDb s (MessageT (QExpr be s))
  -> Q be SlackDb s (MessageT (QExpr be s))
filterQuery = \case
  Query_Day day ->
    let fromDate = UTCTime day 0
        toDate = UTCTime (addDays 1 day) 0
    in filter_ (\msg -> (_messageTs msg >=. val_ fromDate) &&. (_messageTs msg <. val_ toDate))
  Query_Like q -> filter_ (\msg -> (_messageText msg `like_` val_ ("%" <> q <> "%")))


backend :: Backend BackendRoute Route
backend = Backend
  { _backend_routeEncoder = backendRouteEncoder
  , _backend_run = \serve -> do
      Just routeEnv <- liftIO $ fmap T.strip <$> Cfg.get "config/common/route"
      Just oauthClientID <- liftIO $ fmap T.strip <$> Cfg.get "config/backend/oauthClientID"
      Just oauthClientSecret <- liftIO $ fmap T.strip <$> Cfg.get "config/backend/oauthClientSecret"

      liftIO $ T.putStrLn $ "routeEnv: " <> routeEnv
      liftIO populateDatabase
      tlsMgr <- liftIO $ newTlsManager
      (_, sessKey) <- liftIO randomKey
      let defaultPageSize = 30 :: Natural
          mkTautPagination :: Natural -> IO Pagination = liftIO . mkPagination defaultPageSize
          routeToPagination (PaginatedRoute (p, _)) = mkTautPagination p
      let Right (enc :: Encoder Identity Identity (R (Sum BackendRoute (ObeliskRoute Route))) PageName) = checkEncoder backendRouteEncoder

      serve $ \case
        BackendRoute_Missing :/ () -> do
          writeLBS "404"
        BackendRoute_OAuth :/ OAuth_RedirectUri :/ p -> case p of
          Nothing -> liftIO $ error "Expected to receive the authorization code here"
          Just (RedirectUriParams code _mstate) -> do
            let t = TokenRequest
                  { _tokenRequest_grant = TokenGrant_AuthorizationCode $ T.encodeUtf8 code
                  , _tokenRequest_clientId = oauthClientID
                  , _tokenRequest_clientSecret = oauthClientSecret
                  , _tokenRequest_redirectUri = BackendRoute_OAuth
                  }
                reqUrl = "https://slack.com/api/oauth.access"
            -- let reqUrlDev = "https://webhook.site/f29fb6f2-ff15-412d-9e8f-d1b82c4682ad"
            --     routeEnvDev = "http://4ed5742f.ngrok.io"
            req <- liftIO $ getOauthToken reqUrl routeEnv enc t
            r <- liftIO $ flip httpLbs tlsMgr req
            let tokenResponse :: Maybe SlackTokenResponse = decode $ responseBody r
            setAuthToken sessKey $ encode tokenResponse
            liftIO $ putStrLn $ show tokenResponse
        BackendRoute_GetMessages :/ pDay -> do
          resp <- getSlackToken enc oauthClientID routeEnv sessKey >>= \case
            Left e -> pure $ Left e
            Right t -> do
              pagination :: Pagination <- liftIO $ routeToPagination pDay
              resp <- queryMessages (Query_Day $ paginatedRouteValue pDay) $ pagination
              pure $ Right (t, resp)
          writeLBS $ encode resp
        BackendRoute_SearchMessages :/ pQuery -> do
          resp <- getSlackToken enc oauthClientID routeEnv sessKey >>= \case
            Left e -> pure $ Left e
            Right t -> do
              pagination :: Pagination <- liftIO $ routeToPagination pQuery
              resp <- queryMessages (Query_Like $ paginatedRouteValue pQuery) $ pagination
              pure $ Right (t, resp)
          writeLBS $ encode resp
        _ -> undefined -- FIXME: wtf why does the compiler require this?
  }
  where
    getSlackToken enc oauthClientID routeEnv k = getAuthToken k >>= \case
      Nothing -> do
        let r = AuthorizationRequest
              { _authorizationRequest_responseType = AuthorizationResponseType_Code
              , _authorizationRequest_clientId = oauthClientID
              , _authorizationRequest_redirectUri = Nothing -- Just BackendRoute_OAuth
              , _authorizationRequest_scope = ["identity.basic"]
              , _authorizationRequest_state = Just "none"
              }
            authUrl = "https://slack.com/oauth/authorize"
            grantHref = authorizationRequestHref authUrl routeEnv enc r
        pure $ Left grantHref
      Just (_, v) -> do
        let t :: Maybe SlackTokenResponse = decode v
        pure $ Right t
    setAuthToken k t = setSecureCookie "tautAuthToken" Nothing k Nothing (t :: BL.ByteString)
    getAuthToken :: MonadSnap m => Key -> m (Maybe (SecureCookie BL.ByteString))
    getAuthToken k = do
      c <- getsRequest rqCookies
      fmap (join . listToMaybe . catMaybes) $ forM c $ \cc->
        if cookieName cc == "tautAuthToken"
            then let x :: Maybe (SecureCookie BL.ByteString) = decodeSecureCookie @BL.ByteString k (cookieValue cc)
                 in pure $ Just x
            else pure Nothing
    queryMessages q (p :: Pagination) = do
      liftIO $ SQLite.withConnection dbFile $ \conn -> runBeamSqlite conn $ do
        total <- fmap (fromMaybe 0) $ runSelectReturningOne $
          select $ aggregate_ (\_ -> countAll_) $ filterQuery q $ all_ (_slackMessages slackDb)
        msgs :: Paginated Message <- paginate p (fromIntegral total) $ \offset limit ->
          runSelectReturningList $ select $
            limit_ limit $ offset_ offset $
              orderBy_ (asc_ . _messageTs) $ filterQuery q $ all_ (_slackMessages slackDb)
        -- TODO: separate endpoint for this?
        users :: [User] <- runSelectReturningList $
          select $ all_ (_slackUsers slackDb)
        pure (users, msgs)


rootDir :: String
rootDir = "/home/srid/code/Taut/tmp"

dbFile :: String
dbFile = rootDir <> "/data3.sqlite3"

data SlackDb f = SlackDb
  { _slackChannels :: f (TableEntity ChannelT)
  , _slackUsers :: f (TableEntity UserT)
  , _slackMessages :: f (TableEntity MessageT)
  }
  deriving (Generic)

instance Database be SlackDb

slackDb :: DatabaseSettings be SlackDb
slackDb = defaultDbSettings

loadFile :: FromJSON a => String -> IO a
loadFile = BL.readFile >=> either error return . eitherDecode

channelMessages :: Channel -> IO [Message]
channelMessages channel = msgFiles >>= fmap join . traverse loadFile >>= pure . fmap setChannel
  where
    msgFiles = do
      let c = T.unpack $ _channelName channel
      listDirectory (rootDir </> c)
        <&> filter (isSuffixOf ".json")
        <&> fmap ((rootDir </> c) </>)
    setChannel msg = msg  { _messageChannelName = Just $ _channelName channel }

-- TODO: Figure out a better way to do this. beam-migrate?
schema :: [SQLite.Query]
schema =
  [ "CREATE TABLE channels (id VARCHAR NOT NULL, name VARCHAR NOT NULL, created VARCHAR NOT NULL, PRIMARY KEY( id ));"
  , "CREATE TABLE users (id VARCHAR NOT NULL, team_id VARCHAR NOT NULL, name VARCHAR NOT NULL, deleted BOOL NOT NULL, color VARCHAR, real_name VARCHAR, tz VARCHAR, tz_label VARCHAR, tz_offset INTEGER, PRIMARY KEY( id ));"
  , "CREATE TABLE messages (id INTEGER PRIMARY KEY, type VARCHAR NOT NULL, subtype VARCHAR, user VARCHAR, bot_id VARCHAR, text VARCHAR NOT NULL, client_msg_id VARCHAR, ts INT NOT NULL, channel_name VARCHAR);"
  ]

populateDatabase :: IO ()
populateDatabase = do
  users <- loadFile $ rootDir </> "users.json" :: IO [User]
  channels <- loadFile $ rootDir </> "channels.json" :: IO [Channel]

  messages <- fmap join $ traverse channelMessages channels

  doesFileExist dbFile >>= flip when (removeFile dbFile)

  putStrLn $ "Loading " <> show (length messages) <> " messages into " <> dbFile

  SQLite.withConnection dbFile $ \conn -> do
    SQLite.withTransaction conn $ do
      -- Create tables
      forM_ schema $ SQLite.execute_ conn
      -- Load JSON data
      runBeamSqlite conn $ do
        runInsert $
          insert (_slackUsers slackDb) $
          insertValues users
        runInsert $
          insert (_slackChannels slackDb) $
          insertValues channels
        forM_ (chunksOf 100 messages) $ \chunk -> do
          runInsert $
            insert (_slackMessages slackDb) $
            insertExpressions (mkMessageExpr <$> chunk)
  where
    mkMessageExpr :: Message -> MessageT (QExpr Sqlite s)
    mkMessageExpr m = Message
      (val_ $ _messageType m)
      (val_ $ _messageSubtype m)
      (val_ $ _messageUser m)
      (val_ $ _messageBotId m)
      (val_ $ _messageText m)
      (val_ $ _messageClientMsgId m)
      (val_ $ _messageTs m)
      (val_ $ _messageChannelName m)
