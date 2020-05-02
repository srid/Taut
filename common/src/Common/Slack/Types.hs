{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Slack database (beam) types
module Common.Slack.Types where

import Common.Slack.Internal
import Data.Aeson
import Data.Pagination (Paginated, Pagination)
import Data.Text (Text)
import Data.Time.Clock
import Database.Beam

-- | Slack User
data UserT f = User
  { _userId :: Columnar f Text,
    _userTeamId :: Columnar f Text,
    _userName :: Columnar f Text,
    _userDeleted :: Columnar f Bool,
    _userColor :: Columnar f (Maybe Text),
    _userRealName :: Columnar f (Maybe Text),
    _userTz :: Columnar f (Maybe Text),
    _userTzLabel :: Columnar f (Maybe Text),
    _userTzOffset :: Columnar f (Maybe Int)
    -- , _userProfile :: Columnar f Profile
  }
  deriving (Generic)

-- TODO: How to model profile in database?

type User = UserT Identity

type UserId = PrimaryKey UserT Identity

instance Beamable UserT

deriving instance Show User

deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic)
  primaryKey = UserId . _userId

instance Beamable (PrimaryKey UserT)

-- TODO: Image links
-- FIXME: json parser would fail here. argh
data Profile = Profile
  { _profileTitle :: Text,
    _profileAvatarHash :: Text,
    _profileTeam :: Text
  }
  deriving (Eq, Generic, Show)

-- | Slack Channel
data ChannelT f = Channel
  { _channelId :: Columnar f Text,
    _channelName :: Columnar f Text
  }
  deriving (Generic)

type Channel = ChannelT Identity

type ChannelId = PrimaryKey ChannelT Identity

instance Beamable ChannelT

deriving instance Show Channel

deriving instance Show ChannelId

deriving instance Eq Channel

deriving instance Eq ChannelId

instance Table ChannelT where
  data PrimaryKey ChannelT f = ChannelId (Columnar f Text) deriving (Generic)
  primaryKey = ChannelId . _channelId

instance Beamable (PrimaryKey ChannelT)

-- | Slack Message
-- XXX: "user" is not always present; eg. for bots, which have "bot_id" and "subtype"
data MessageT f = Message
  { _messageType :: Columnar f Text,
    _messageSubtype :: Columnar f (Maybe Text),
    _messageUser :: Columnar f (Maybe Text), -- Join with User ID
    _messageUserName :: Columnar f (Maybe Text), -- Because I'm lazy to do a join. :-P
    _messageBotId :: Columnar f (Maybe Text),
    _messageText :: Columnar f Text,
    _messageClientMsgId :: Columnar f (Maybe Text), -- XXX: This can be empty?
    _messageTs :: Columnar f UTCTime,
    -- Ideally this column should be a foreign key (via Beam's PrimaryKey), but that interferes with json
    -- parsing, so we will just keep it stupid and copy the channel name in here.
    _messageChannelName :: Columnar f (Maybe Text)
  }
  deriving (Generic)

type Message = MessageT Identity

type MessageId = PrimaryKey MessageT Identity

instance Beamable MessageT

deriving instance Show Message

deriving instance Eq Message

-- Timestamps in Slack export archives are precise enough (eg:
-- "1514808718.000015") to be treated as primary keys, such as to allow us to
-- use them in the URL paths to access specific messages. See however the note in
-- Internal.hs about possible invalidation of this quality during convertion to UTCTime.
instance Table MessageT where
  data PrimaryKey MessageT f = MessageId (Columnar f UTCTime) deriving (Generic)
  primaryKey = MessageId . _messageTs

instance Beamable (PrimaryKey MessageT)

instance FromJSON User where
  parseJSON = genericParseJSON fieldLabelMod

instance FromJSON Profile where
  parseJSON = genericParseJSON fieldLabelMod

instance FromJSON Channel where
  parseJSON = genericParseJSON fieldLabelMod

instance FromJSON Message where
  parseJSON = withObject "message" $ \o -> do
    type_ <- o .: "type"
    subtype <- o .:? "subtype"
    user <- o .:? "user"
    username <- o .:? "user_name"
    botid <- o .:? "bot_id"
    txt <- o .: "text"
    msgid <- o .:? "client_msg_id"
    ts <- parseSlackTimestamp =<< o .: "ts"
    channelName <- o .:? "channel_name"
    pure $ Message type_ subtype user username botid txt msgid ts channelName

instance ToJSON Message where
  toJSON m =
    object
      [ "type" .= _messageType m,
        "subtype" .= _messageSubtype m,
        "user" .= _messageUser m,
        "user_name" .= _messageUserName m,
        "bot_id" .= _messageBotId m,
        "text" .= _messageText m,
        "client_msg_id" .= _messageClientMsgId m,
        "ts" .= formatSlackTimestamp (_messageTs m),
        "channel_name" .= _messageChannelName m
      ]

instance ToJSON User where
  toJSON = genericToJSON fieldLabelMod

instance ToJSON Profile where
  toJSON = genericToJSON fieldLabelMod

instance ToJSON Channel where
  toJSON = genericToJSON fieldLabelMod

instance ToJSON Pagination

instance ToJSON (Paginated Message)

instance FromJSON Pagination

instance FromJSON (Paginated Message)
