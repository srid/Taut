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

import Codec.Archive.Zip
import Common.Slack.Types
import Common.Slack.Types.Auth (SlackTeam (..))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam
import Database.Beam.Sqlite (Sqlite, runBeamSqlite)
import qualified Database.SQLite.Simple as SQLite

data SlackDb f = SlackDb
  { _slackChannels :: f (TableEntity ChannelT),
    _slackUsers :: f (TableEntity UserT),
    _slackMessages :: f (TableEntity MessageT)
  }
  deriving (Generic)

instance Database be SlackDb

slackDb :: DatabaseSettings be SlackDb
slackDb = defaultDbSettings

loadFromArchive :: FromJSON a => EntrySelector -> ZipArchive a
loadFromArchive = either error pure . eitherDecode <=< fmap BL.fromStrict . getEntry

channelMessages :: Channel -> ZipArchive [Message]
channelMessages channel =
  fmap setChannel <$> (msgFiles >>= fmap join . traverse loadFromArchive)
  where
    msgFiles = do
      let c = _channelName channel
      entries <- Map.keys <$> getEntries
      pure $ flip filter entries $ \e ->
        let name = getEntryName e in T.isPrefixOf (c <> "/") name && T.isSuffixOf ".json" name
    setChannel msg = msg {_messageChannelName = Just $ _channelName channel}

schema :: [SQLite.Query]
schema =
  [ "CREATE TABLE channels (id VARCHAR NOT NULL, name VARCHAR NOT NULL, PRIMARY KEY( id ));",
    "CREATE TABLE users (id VARCHAR NOT NULL, team_id VARCHAR NOT NULL, name VARCHAR NOT NULL, deleted BOOL NOT NULL, color VARCHAR, real_name VARCHAR, tz VARCHAR, tz_label VARCHAR, tz_offset INTEGER, PRIMARY KEY( id ));",
    "CREATE TABLE messages (id INTEGER PRIMARY KEY, type VARCHAR NOT NULL, subtype VARCHAR, user VARCHAR, user_name VARCHAR, bot_id VARCHAR, text VARCHAR NOT NULL, client_msg_id VARCHAR, ts INT NOT NULL, channel_name VARCHAR);"
  ]

populateDatabase :: SQLite.Connection -> Text -> IO SlackTeam
populateDatabase conn archivePath = do
  (users, channels, messages) <- withArchive (T.unpack archivePath) $ do
    users :: [User] <- loadFromArchive =<< mkEntrySelector "users.json"
    channels :: [Channel] <- loadFromArchive =<< mkEntrySelector "channels.json"
    messages <- fmap join $ traverse channelMessages channels
    pure (users, channels, messages)
  let Just (team :: SlackTeam) = fmap SlackTeam $ listToMaybe $ _userTeamId <$> users
  let userMap = Map.fromList $ flip fmap users $ \u -> (_userId u, _userName u)
  SQLite.withTransaction conn $ do
    -- Create tables
    forM_ schema $ SQLite.execute_ conn
    -- Load JSON data
    runBeamSqlite conn $ do
      forM_ (chunksOf 100 users) $ \chunk -> do
        runInsert
          $ insert (_slackUsers slackDb)
          $ insertValues chunk
      forM_ (chunksOf 100 channels) $ \chunk -> do
        runInsert
          $ insert (_slackChannels slackDb)
          $ insertValues chunk
      forM_ (chunksOf 100 messages) $ \chunk -> do
        runInsert
          $ insert (_slackMessages slackDb)
          $ insertExpressions (mkMessageExpr userMap <$> chunk)
  putStrLn $ "Loaded " <> show (length messages) <> " messages into memory "
  pure team
  where
    mkMessageExpr :: Map Text Text -> Message -> MessageT (QExpr Sqlite s)
    mkMessageExpr users m =
      Message
        (val_ $ _messageType m)
        (val_ $ _messageSubtype m)
        (val_ $ _messageUser m)
        (val_ $ flip Map.lookup users =<< _messageUser m)
        (val_ $ _messageBotId m)
        (val_ $ _messageText m)
        (val_ $ _messageClientMsgId m)
        (val_ $ _messageTs m)
        (val_ $ _messageChannelName m)
