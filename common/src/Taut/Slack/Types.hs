{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Taut.Slack.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)

import Database.Beam

import Taut.Slack.Internal

data User = User
  { _userId :: Text
  , _userTeamId :: Text
  , _username :: Text
  , _userDeleted :: Bool
  , _userColor :: Maybe Text
  , _userRealName :: Maybe Text
  , _userTz :: Maybe Text
  , _userTzLabel :: Maybe Text
  , _userTzOffset :: Maybe Int
  , _userProfile :: Profile
  }
  deriving (Eq, Generic, Show)

-- TODO: Image links
-- FIXME: json parser would fail here. argh
data Profile = Profile
  { _profileTitle :: Text
  , _profileAvatarHash :: Text
  , _profileTeam :: Text
  }
  deriving (Eq, Generic, Show)

data ChannelT f = Channel
  { _channelId :: Columnar f Text
  , _channelName :: Columnar f Text
  , _channelCreated :: Columnar f Text -- Uh
  }
  deriving (Generic)

type Channel = ChannelT Identity
type ChannelId = PrimaryKey ChannelT Identity

instance Beamable ChannelT
deriving instance Show Channel
deriving instance Eq Channel

instance Table ChannelT where
  data PrimaryKey ChannelT f = ChannelId (Columnar f Text) deriving Generic
  primaryKey = ChannelId . _channelId
instance Beamable (PrimaryKey ChannelT)

data Message = Message
  { _messageType :: Text
  , _messageUser :: Text -- Join with User ID
  , _messageText :: Text
  , _messageClientMsgId :: Maybe Text -- XXX: maybe
  , _messageTs :: Text -- Timestamp, I think
  }
  deriving (Eq, Generic, Show)

instance FromJSON User where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON Profile where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON Channel where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON Message where
  parseJSON = genericParseJSON fieldLabelMod

instance ToJSON User where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON Profile where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON Channel where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON Message where
  toJSON = genericToJSON fieldLabelMod
