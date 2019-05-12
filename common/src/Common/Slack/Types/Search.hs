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
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Generics

data MessageFilters = MessageFilters
  { _messageFilters_terms :: [Text]
  , _messageFilters_from :: [Text]
  , _messageFilters_in :: [Text]
  , _messageFilters_during :: [Day]
  , _messageFilters_after :: Maybe Day
  , _messageFilters_before :: Maybe Day
  , _messageFilters_at :: Maybe UTCTime
  , _messageFilters_hasPin :: Maybe ()
  }
  deriving (Eq, Show, Generic)

makeLenses ''MessageFilters

instance Default MessageFilters where
  def = MessageFilters [] [] [] [] Nothing Nothing Nothing Nothing

instance FromJSON MessageFilters
instance ToJSON MessageFilters

isOnlyDuring :: MessageFilters -> Maybe Day
isOnlyDuring mf = do
  firstDay <- listToMaybe (mf ^. messageFilters_during)
  let mf' = def & messageFilters_during .~ [firstDay]
  guard $ mf == mf'
  pure firstDay
