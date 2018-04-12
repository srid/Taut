{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List (isSuffixOf)
import Data.List.Split (chunksOf)
import Data.Monoid ((<>))
import qualified Data.Text as T
import System.Directory

import Control.Lens.Operators ((<&>))
import Database.Beam
import Database.Beam.Sqlite ()
import qualified Database.SQLite.Simple as SQLite

import Taut.Slack.Types

rootDir :: String
rootDir = "/home/srid/tmp/data"

data SlackDb f = SlackDb
  { _slackChannels :: f (TableEntity ChannelT)
  , _slackUsers :: f (TableEntity UserT)
  , _slackMessages :: f (TableEntity MessageT)
  }
  deriving (Generic)

instance Database SlackDb

slackDb :: DatabaseSettings be SlackDb
slackDb = defaultDbSettings

loadFile :: FromJSON a => String -> IO a
loadFile = B.readFile . (rootDir <>) >=> either error return . eitherDecode

channelMessageFiles :: Channel -> IO [String]
channelMessageFiles channel = do
  -- FIXME: Replace joining with "/" using library function
  let channelName = T.unpack $ _channelName channel
  listDirectory (rootDir <> "/" <> channelName)
    <&> filter (isSuffixOf ".json")
    <&> fmap (("/" <> channelName <> "/") <>)

channelMessages :: Channel -> IO [Message]
channelMessages = channelMessageFiles >=> fmap join . traverse loadFile

main :: IO ()
main = do
  users <- loadFile "/users.json" :: IO [User]
  channels <- loadFile "/channels.json" :: IO [Channel]

  messages <- fmap join $ traverse channelMessages channels

  let dbFile = rootDir <> "/data.sqlite3"
  doesFileExist dbFile >>= flip when (removeFile dbFile)

  putStrLn $ "Loading " <> show (length messages) <> " messages into " <> dbFile

  SQLite.withConnection dbFile $ \conn -> do
    SQLite.withTransaction conn $ do
      SQLite.execute_ conn "CREATE TABLE channels (id VARCHAR NOT NULL, name VARCHAR NOT NULL, created VARCHAR NOT NULL, PRIMARY KEY( id ));"
      SQLite.execute_ conn "CREATE TABLE users (id VARCHAR NOT NULL, team_id VARCHAR NOT NULL, name VARCHAR NOT NULL, deleted BOOL NOT NULL, color VARCHAR, real_name VARCHAR, tz VARCHAR, tz_label VARCHAR, tz_offset INTEGER, PRIMARY KEY( id ));"
      SQLite.execute_ conn "CREATE TABLE messages (id INTEGER PRIMARY KEY, type VARCHAR NOT NULL, subtype VARCHAR, user VARCHAR, bot_id VARCHAR, text VARCHAR NOT NULL, client_msg_id VARCHAR, ts VARCHAR NOT NULL);"

      withDatabase conn $ do
        runInsert $
          insert (_slackUsers slackDb) $
          insertValues users
        runInsert $
          insert (_slackChannels slackDb) $
          insertValues channels
        forM_ (chunksOf 100 messages) $ \chunk -> do
          runInsert $
            insert (_slackMessages slackDb) $
            insertValues chunk
