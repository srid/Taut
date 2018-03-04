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
  (Right channels) <- loadFile "/channels.json" :: IO (Either String [Channel])
  -- (Right channels) <- loadFile "/general/2018-02-15.json" :: IO (Either String [Message])

  putStrLn . show $ channels

  let dbFile = rootDir <> "slack.db"

  doesFileExist dbFile >>= flip when (removeFile dbFile)

  SQLite.withConnection dbFile $ \conn -> do
    SQLite.execute_ conn "CREATE TABLE channels (id VARCHAR NOT NULL, name VARCHAR NOT NULL, created VARCHAR NOT NULL, PRIMARY KEY( id ));"

    withDatabaseDebug putStrLn conn $ do
      runInsert $
        insert (_slackChannels slackDb) $
        insertValues channels
