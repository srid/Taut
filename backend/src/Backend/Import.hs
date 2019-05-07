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
-- Import Slack JSON archive into database
module Backend.Import where

import Control.Lens.Operators ((<&>))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf)
import Data.List.Split (chunksOf)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))

import Database.Beam
import Database.Beam.Sqlite (Sqlite, runBeamSqlite)
import qualified Database.SQLite.Simple as SQLite

import Common.Slack.Types
import Common.Slack.Types.Auth (SlackTeam (..))

rootDir :: String
rootDir = "/home/srid/code/Taut/tmp"

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

populateDatabase :: SQLite.Connection -> IO SlackTeam
populateDatabase conn = do
  users <- loadFile $ rootDir </> "users.json" :: IO [User]
  channels <- loadFile $ rootDir </> "channels.json" :: IO [Channel]
  let Just (team :: SlackTeam) = fmap SlackTeam $ listToMaybe $ _userTeamId <$> users

  messages <- fmap join $ traverse channelMessages channels

  putStrLn $ "Loading " <> show (length messages) <> " messages into memory "

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
