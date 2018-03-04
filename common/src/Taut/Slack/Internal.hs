{-# LANGUAGE OverloadedStrings #-}

module Taut.Slack.Internal (fieldLabelMod) where

import Data.Aeson
import Data.Char (isUpper)
import Text.Casing (fromHumps, toQuietSnake)

-- | Field label modifier for converting Slack datatypes to json
fieldLabelMod :: Options
fieldLabelMod = defaultOptions
  { fieldLabelModifier = toQuietSnake . fromHumps . dropWhile (not . isUpper)
  }
