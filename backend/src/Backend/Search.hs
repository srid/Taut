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
import Common.Slack.Types.Search

import Backend.Import (SlackDb (..))
import Backend.Search.Parser (SearchKeyword(..), SearchModifier(..))


mkMessageFilters :: [Either SearchModifier SearchKeyword] -> MessageFilters
mkMessageFilters = foldl f allMessages
  where
    f mf = \case
      Right kw -> mf & messageFilters_terms %~ (unSearchKeyword kw:)
      Left md -> case md of
        SearchModifier_From user -> mf & messageFilters_from %~ (user:)
        SearchModifier_In chan -> mf & messageFilters_in %~ (chan:)
        SearchModifier_During day -> mf & messageFilters_during %~ (day:)
        SearchModifier_After day -> mf & messageFilters_after ?~ day
        SearchModifier_Before day -> mf & messageFilters_before ?~ day
        SearchModifier_At t -> mf & messageFilters_at ?~ t
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
      msgContaining msg <$> mf ^. messageFilters_terms
  , maybe Nothing (Just . foldl1 (||.)) $ NEL.nonEmpty $
      msgFrom msg <$> mf ^. messageFilters_from
  , maybe Nothing (Just . foldl1 (||.)) $ NEL.nonEmpty $
      msgDuring msg <$> mf ^. messageFilters_during
  , maybe Nothing (Just . foldl1 (||.)) $ NEL.nonEmpty $
      msgInChannel msg <$> mf ^. messageFilters_in
  , msgAt msg <$> mf ^. messageFilters_at
  , msgAfter msg <$> mf ^. messageFilters_after
  , msgBefore msg <$> mf ^. messageFilters_before
  ]
  where
    msgContaining msg q = _messageText msg `like_` val_ ("%" <> q <> "%")
    msgFrom msg (user :: Text) = _messageUserName msg ==. just_ (val_ user)
    msgInChannel msg (ch :: Text) = _messageChannelName msg ==. just_ (val_ ch)
    msgDuring msg day = _messageTs msg >=. val_ (dayStart day) &&. _messageTs msg <. val_ (dayEnd day)
    msgAt msg t = _messageTs msg >=. val_ t
    msgAfter msg day = _messageTs msg >=. val_ (dayStart day)
    msgBefore msg day = _messageTs msg <=. val_ (dayEnd day)

    dayStart day = UTCTime day 0
    dayEnd day = UTCTime (addDays 1 day) 0
