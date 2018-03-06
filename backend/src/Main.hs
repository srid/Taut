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
loadFile path = B.readFile (rootDir <> path) >>= handleLeft . eitherDecode
  where handleLeft (Left e) = error e
        handleLeft (Right v) = return v

main :: IO ()
main = do
  users <- loadFile "/users.json" :: IO [User]
  channels <- loadFile "/channels.json" :: IO [Channel]

  messageFiles <- fmap join $ forM channels $ \channel -> do
    let channelName = T.unpack $ _channelName channel
    listDirectory (rootDir <> "/" <> channelName)
      >>= return . filter (isSuffixOf ".json")
      >>= return . fmap (\p -> "/" <> channelName <> "/" <> p)
  messages :: [Message] <- fmap join $ forM messageFiles $ \path -> do
    putStrLn $ "Loading " <> path
    loadFile path

  let dbFile = rootDir <> "/data.sqlite3"
  doesFileExist dbFile >>= flip when (removeFile dbFile)

  SQLite.withConnection dbFile $ \conn -> do
    SQLite.withTransaction conn $ do
      SQLite.execute_ conn "CREATE TABLE channels (id VARCHAR NOT NULL, name VARCHAR NOT NULL, created VARCHAR NOT NULL, PRIMARY KEY( id ));"
      SQLite.execute_ conn "CREATE TABLE users (id VARCHAR NOT NULL, team_id VARCHAR NOT NULL, name VARCHAR NOT NULL, deleted BOOL NOT NULL, color VARCHAR, real_name VARCHAR, tz VARCHAR, tz_label VARCHAR, tz_offset INTEGER, PRIMARY KEY( id ));"
      SQLite.execute_ conn "CREATE TABLE messages (type VARCHAR NOT NULL, subtype VARCHAR, user VARCHAR, bot_id VARCHAR, text VARCHAR NOT NULL, client_msg_id VARCHAR, ts VARCHAR NOT NULL, PRIMARY KEY( ts ));"

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
