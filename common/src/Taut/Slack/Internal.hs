{-# LANGUAGE OverloadedStrings #-}

module Taut.Slack.Internal (fieldLabelMod) where

import Data.Aeson
import Data.Aeson.Types
import Data.List (elemIndices)
import Text.Casing (fromHumps, toQuietSnake)

-- | Field label modifier for converting Slack datatypes to json
fieldLabelMod :: Options
fieldLabelMod = defaultOptions
  { fieldLabelModifier = toQuietSnake . fromHumps . stripRecordPrefix
  }

-- XXX: This will runtime error out unless the field name is of the format `_{recordName}_{fieldName}`
-- Ideally we should figure out a way to enforce this constraint in the compiler.
stripRecordPrefix :: String -> String
stripRecordPrefix s = drop n s
  where n = 1 + (elemIndices '_' s !! 1)
