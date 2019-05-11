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
  , msgAt msg <$> mf ^. messageFilters_at
  ]
  where
    msgContaining msg q = _messageText msg `like_` val_ ("%" <> q <> "%")
    msgFrom msg (user :: Text) = _messageUserName msg ==. just_ (val_ user) -- FIXME: join with userName
    msgDuring msg day = _messageTs msg >=. val_ fromTs &&. _messageTs msg <. val_ toTs
      where
        fromTs = UTCTime day 0
        toTs = UTCTime (addDays 1 day) 0
    msgAt msg t = _messageTs msg >=. val_ t
