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

import Control.Exception.Safe (throwString)
import Control.Lens.Operators ((<&>))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time.Calendar
import Data.Time.Clock
import System.Directory
import System.FilePath ((</>))

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite (Sqlite, runBeamSqlite)
import qualified Database.SQLite.Simple as SQLite
import Network.HTTP.Client

import Data.Pagination
import Snap

import Obelisk.Backend as Ob
import Obelisk.OAuth.AccessToken
import Obelisk.OAuth.Authorization
import Obelisk.Route hiding (decode, encode)

import Common.Route
import Common.Slack.Types
import Common.Slack.Types.Auth (SlackTeam(..))

import Backend.Config
import Backend.Login

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
      team <- liftIO populateDatabase
      cfg <- readBackendConfig team
      liftIO $ T.putStrLn $ "teamID: " <> T.pack (show $ _backendConfig_team cfg)
      liftIO $ T.putStrLn $ "routeEnv: " <> _backendConfig_routeEnv cfg

      serve $ \case
        BackendRoute_Missing :/ () -> do
          writeLBS "404"
        BackendRoute_OAuth :/ OAuth_RedirectUri :/ p -> case p of
          Nothing -> liftIO $ throwString "Expected to receive the authorization code here"
          Just (RedirectUriParams code mstate) -> do
            let t = TokenRequest
                  { _tokenRequest_grant = TokenGrant_AuthorizationCode $ T.encodeUtf8 code
                  , _tokenRequest_clientId = _backendConfig_oauthClientID cfg
                  , _tokenRequest_clientSecret = _backendConfig_oauthClientSecret cfg
                  , _tokenRequest_redirectUri = BackendRoute_OAuth
                  }
                reqUrl = "https://slack.com/api/oauth.access"
            -- let reqUrlDev = "https://webhook.site/f29fb6f2-ff15-412d-9e8f-d1b82c4682ad"
            --     routeEnvDev = "http://4ed5742f.ngrok.io"
            req <- liftIO $ getOauthToken
              reqUrl
              (_backendConfig_routeEnv cfg)
              (_backendConfig_enc cfg)
              t
            resp <- liftIO $ httpLbs req $ _backendConfig_tlsMgr cfg
            -- TODO: check response errors (both code and body json)!
            case decode (responseBody resp) of
              Nothing -> liftIO $ throwString "Unable to decode JSON from Slack oauth.access response"
              Just str -> do
                setSlackTokenToCookie cfg str
                redirect $ T.encodeUtf8 $ fromMaybe (renderFrontendRoute (_backendConfig_enc cfg) $ Route_Home :/ ()) mstate
        BackendRoute_GetMessages :/ pDay -> do
          -- TODO: Use MonadError wherever possible
          resp <- authorizeUser cfg (Route_Messages :/ pDay) >>= \case
            Left e -> pure $ Left e
            Right t -> do
              pagination <- liftIO $ mkPaginationFromRoute cfg pDay
              resp <- queryMessages (Query_Day $ paginatedRouteValue pDay) $ pagination
              pure $ Right (t, resp)
          writeLBS $ encode resp
        BackendRoute_SearchMessages :/ pQuery -> do
          resp <- authorizeUser cfg (Route_Search :/ pQuery) >>= \case
            Left e -> pure $ Left e
            Right t -> do
              pagination <- liftIO $ mkPaginationFromRoute cfg pQuery
              resp <- queryMessages (Query_Like $ paginatedRouteValue pQuery) $ pagination
              pure $ Right (t, resp)
          writeLBS $ encode resp
        _ -> undefined -- FIXME: wtf why does the compiler require this?
  }
  where
    mkPaginationFromRoute cfg p = mkPagination (_backendConfig_pageSize cfg) (paginatedRoutePageIndex p)

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

-- TODO: Move these to a separate module (ArchiveImport.hs?)

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

populateDatabase :: IO SlackTeam
populateDatabase = do
  users <- loadFile $ rootDir </> "users.json" :: IO [User]
  channels <- loadFile $ rootDir </> "channels.json" :: IO [Channel]
  let Just (team :: SlackTeam) = fmap SlackTeam $ listToMaybe $ _userTeamId <$> users

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
  pure team
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
