{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Slack.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

import Taut.Slack.Internal

data User = User
  { _user_id :: Text
  , _user_teamId :: Text
  , _user_name :: Text
  , _user_deleted :: Bool
  , _user_color :: Maybe Text
  , _user_realName :: Maybe Text
  , _user_tz :: Maybe Text
  , _user_tzLabel :: Maybe Text
  , _user_tzOffset :: Maybe Int
  , _user_profile :: UserProfile
  }
  deriving (Eq, Generic, Show)

-- TODO: Image links
data UserProfile = UserProfile
  { _userProfile_title :: Text
  , _userProfile_avatarHash :: Text
  , _userProfile_team :: Text
  }
  deriving (Eq, Generic, Show)

data Channel = Channel
  { _channel_id :: Text
  }

instance FromJSON User where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON UserProfile where
  parseJSON = genericParseJSON fieldLabelMod

instance ToJSON User where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON UserProfile where
  toJSON = genericToJSON fieldLabelMod
