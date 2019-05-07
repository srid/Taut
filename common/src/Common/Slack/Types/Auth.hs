{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Types for working with Slack's OAuth interface
module Common.Slack.Types.Auth where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

import Text.Casing (fromHumps, toQuietSnake)

data SlackTokenResponse = SlackTokenResponse
  { _slackTokenResponse_ok :: Bool
  , _slackTokenResponse_accessToken :: Text
  , _slackTokenResponse_scope :: Text
  , _slackTokenResponse_user :: SlackUser
  , _slackTokenResponse_team :: SlackTeam
  }
  deriving (Eq, Show, Generic)

data SlackUser = SlackUser
  { _slackUser_name :: Text
  , _slackUser_id :: Text
  }
  deriving (Eq, Show, Generic)

data SlackTeam = SlackTeam
  { _slackTeam_id :: Text
  }
  deriving (Eq, Show, Generic)

data NotAuthorized
  = NotAuthorized_RequireLogin Text
  | NotAuthorized_WrongTeam SlackTeam Text
  deriving (Eq, Show, Generic)

-- | Slack's OAuth JSON field label modifier
fieldLabelMod :: Options
fieldLabelMod = defaultOptions
  { fieldLabelModifier =
      toQuietSnake . fromHumps . drop 1 . dropWhile (/= '_') . drop 1
  }

instance FromJSON SlackTokenResponse where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON SlackUser where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON SlackTeam where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON NotAuthorized where
  parseJSON = genericParseJSON fieldLabelMod

instance ToJSON SlackTokenResponse where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON SlackTeam where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON SlackUser where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON NotAuthorized where
  toJSON = genericToJSON fieldLabelMod
