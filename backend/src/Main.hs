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
import Data.Monoid ((<>))
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
  }
  deriving (Generic)

instance Database SlackDb

slackDb :: DatabaseSettings be SlackDb
slackDb = defaultDbSettings

loadFile :: FromJSON a => String -> IO (Either String a)
loadFile path = B.readFile (rootDir <> path) >>= return . eitherDecode

loadUsers :: IO (Either String [User])
loadUsers = do
  usersData <- B.readFile "/home/srid/tmp/data/users.json"
  return $ (eitherDecode usersData :: Either String [User])

main :: IO ()
main = do
  -- TODO: other types
  (Right users) <- loadFile "/users.json" :: IO (Either String [User])
  (Right channels) <- loadFile "/channels.json" :: IO (Either String [Channel])
  -- (Right channels) <- loadFile "/general/2018-02-15.json" :: IO (Either String [Message])

  let dbFile = rootDir <> "/data.sqlite3"

  doesFileExist dbFile >>= flip when (removeFile dbFile)

  SQLite.withConnection dbFile $ \conn -> do
    SQLite.withTransaction conn $ do
      SQLite.execute_ conn "CREATE TABLE channels (id VARCHAR NOT NULL, name VARCHAR NOT NULL, created VARCHAR NOT NULL, PRIMARY KEY( id ));"
      SQLite.execute_ conn "CREATE TABLE users (id VARCHAR NOT NULL, team_id VARCHAR NOT NULL, name VARCHAR NOT NULL, deleted BOOL NOT NULL, color VARCHAR, real_name VARCHAR, tz VARCHAR, tz_label VARCHAR, tz_offset INTEGER, PRIMARY KEY( id ));"

      withDatabaseDebug putStrLn conn $ do
        runInsert $
          insert (_slackUsers slackDb) $
          insertValues users
        runInsert $
          insert (_slackChannels slackDb) $
          insertValues channels
