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

data UserT f = User
  { _userId :: Columnar f Text
  , _userTeamId :: Columnar f Text
  , _userName :: Columnar f Text
  , _userDeleted :: Columnar f Bool
  , _userColor :: Columnar f (Maybe Text)
  , _userRealName :: Columnar f (Maybe Text)
  , _userTz :: Columnar f (Maybe Text)
  , _userTzLabel :: Columnar f (Maybe Text)
  , _userTzOffset :: Columnar f (Maybe Int)
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
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
  primaryKey = UserId . _userId
instance Beamable (PrimaryKey UserT)

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
