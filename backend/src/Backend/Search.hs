{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Backend.Search where

import Control.Lens hiding ((<.))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock

import Database.Beam
import Database.Beam.Backend.SQL

import Common.Slack.Types

import Backend.Import (SlackDb (..))
import Backend.Search.Parser (SearchKeyword(..), SearchModifier(..))

data MessageFilters = MessageFilters
  { _messageFilters_terms :: [SearchKeyword]
  , _messageFilters_from :: [Text]
  , _messageFilters_in :: [Text]
  , _messageFilters_during :: [Day]
  , _messageFilters_hasPin :: Maybe ()
  }
  deriving (Show, Generic)

makeLenses ''MessageFilters

allMessages :: MessageFilters
allMessages = MessageFilters [] [] [] [] Nothing

mkMessageFilters :: [Either SearchModifier SearchKeyword] -> MessageFilters
mkMessageFilters = foldl f allMessages
  where
    f mf = \case
      Right kw -> mf & messageFilters_terms %~ (kw:)
      Left md -> case md of
        SearchModifier_From user -> mf & messageFilters_from %~ (user:)
        SearchModifier_In chan -> mf & messageFilters_in %~ (chan:)
        SearchModifier_During day -> mf & messageFilters_during %~ (day:)
        SearchModifier_HasPin -> mf & messageFilters_hasPin ?~ ()

messageFilters
  :: forall be s.
     ( BeamSqlBackend be
     , BeamSqlBackendIsString be Text
     , HasSqlValueSyntax (BeamSqlBackendValueSyntax be) Text
     , HasSqlEqualityCheck be Text
     , HasSqlValueSyntax (BeamSqlBackendValueSyntax be) UTCTime
     )
  => MessageFilters
  -> Q be SlackDb s (MessageT (QExpr be s))
  -> Q be SlackDb s (MessageT (QExpr be s))
messageFilters mf = filter_ $ \msg -> foldl (&&.) (val_ True) $ catMaybes $
  [ maybe Nothing (Just . foldl1 (&&.)) $ NEL.nonEmpty $
      msgContaining msg . unSearchKeyword <$> mf ^. messageFilters_terms
  , maybe Nothing (Just . foldl1 (||.)) $ NEL.nonEmpty $
      msgFrom msg <$> mf ^. messageFilters_from
  , maybe Nothing (Just . foldl1 (||.)) $ NEL.nonEmpty $
      msgDuring msg <$> mf ^. messageFilters_during
  ]
  where
    msgContaining msg q = _messageText msg `like_` val_ ("%" <> q <> "%")
    msgFrom msg (user :: Text) = _messageUserName msg ==. just_ (val_ user) -- FIXME: join with userName
    msgDuring msg day = _messageTs msg >=. val_ fromTs &&. _messageTs msg <. val_ toTs
      where
        fromTs = UTCTime day 0
        toTs = UTCTime (addDays 1 day) 0
