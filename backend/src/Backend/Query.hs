{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend.Query where

import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock

import Backend.Import (SlackDb (..))
import Database.Beam
import Database.Beam.Backend.SQL

import Common.Slack.Types

data Query
  = Query_Day Day
  | Query_Like Text

filterQuery
  :: forall be s.
     ( BeamSqlBackend be
     , BeamSqlBackendIsString be Text
     , HasSqlValueSyntax (BeamSqlBackendValueSyntax be) Text
     , HasSqlValueSyntax (BeamSqlBackendValueSyntax be) UTCTime
     )
  => Query
  -> Q be SlackDb s (MessageT (QExpr be s))
  -> Q be SlackDb s (MessageT (QExpr be s))
filterQuery = \case
  Query_Day day ->
    let fromDate = UTCTime day 0
        toDate = UTCTime (addDays 1 day) 0
    in filter_ (\msg -> (_messageTs msg >=. val_ fromDate) &&. (_messageTs msg <. val_ toDate))
  Query_Like q -> filter_ (\msg -> (_messageText msg `like_` val_ ("%" <> q <> "%")))
