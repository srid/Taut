{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Backend where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Data.List (isSuffixOf)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import System.Directory
import System.FilePath ((</>))
import GHC.Natural (Natural)

import Control.Lens.Operators ((<&>))
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite (Sqlite, runBeamSqlite)
import qualified Database.SQLite.Simple as SQLite

import Snap
import Data.Pagination

import Obelisk.Backend as Ob

import Common.Route
import Common.Slack.Types

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
      liftIO populateDatabase
      let defaultPageSize = 10 :: Natural -- TODO: increase
          mkTautPagination :: Natural -> IO Pagination = liftIO . mkPagination defaultPageSize
          routeToPagination (PaginatedRoute (p, _)) = mkTautPagination p
      serve $ \case
        BackendRoute_Missing :=> Identity () -> do
          writeLBS "404"
        BackendRoute_GetMessages :=> Identity pDay -> do
          pagination :: Pagination <- liftIO $ routeToPagination pDay
          resp <- queryMessages (Query_Day $ paginatedRouteValue pDay) $ pagination
          writeLBS $ encode resp
        BackendRoute_SearchMessages :=> Identity pQuery -> do
          pagination :: Pagination <- liftIO $ routeToPagination pQuery
          resp <- queryMessages (Query_Like $ paginatedRouteValue pQuery) $ pagination
          writeLBS $ encode resp
  }
  where
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
dbFile = rootDir <> "/data2.sqlite3"

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
loadFile = B.readFile >=> either error return . eitherDecode

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
