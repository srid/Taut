{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Slack.Types.Search where

import Control.Lens hiding ((<.))
import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar
import GHC.Generics

data MessageFilters = MessageFilters
  { _messageFilters_terms :: [Text]
  , _messageFilters_from :: [Text]
  , _messageFilters_in :: [Text]
  , _messageFilters_during :: [Day]
  , _messageFilters_hasPin :: Maybe ()
  }
  deriving (Eq, Show, Generic)

makeLenses ''MessageFilters

instance FromJSON MessageFilters
instance ToJSON MessageFilters

allMessages :: MessageFilters
allMessages = MessageFilters [] [] [] [] Nothing

isOnlyDuring :: MessageFilters -> Maybe Day
isOnlyDuring mf = do
  firstDay <- listToMaybe (mf ^. messageFilters_during)
  let mf' = allMessages & messageFilters_during .~ [firstDay]
  guard $ mf == mf'
  pure firstDay
