{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend.Query where

import Data.Foldable (foldr1)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Void

import Database.Beam
import Database.Beam.Backend.SQL
import Text.Megaparsec
import Text.Megaparsec.Char
-- import qualified Text.Megaparsec.Char.Lexer as L

import Common.Slack.Types

import Backend.Import (SlackDb (..))

data SearchModifier
  = SearchModifier_From Text  -- ^ Sent by person
  | SearchModifier_In Text  -- ^ In channel
  | SearchModifier_Before Day
  | SearchModifier_After Day
  -- | SearchModifier_During (Either Day (Either Int Int)  -- On day, month or year.
  deriving Show

newtype SearchKeyword = SearchKeyword Text
  deriving Show

type Parser = Parsec Void Text

searchModifier :: Parser SearchModifier
searchModifier =
      try (SearchModifier_From <$> attribute "from")
  <|> try (SearchModifier_In <$> attribute "in")

searchKeyword :: Parser SearchKeyword
searchKeyword = SearchKeyword . T.pack <$> some alphaNumChar  -- TODO: support quoted strings

attribute :: Text -> Parser Text
attribute name = do
  _ <- string name
  _ <- string ":"
  T.pack <$> some alphaNumChar

demo :: IO ()
demo = parseTest (sepBy (Left <$> try searchModifier <|> Right <$> searchKeyword) space1) "from:srid hello world  in:general more  query"

data MessageFilters = MessageFilters
  { _c_terms :: [Text]
  , _c_from :: [Text]
  , _c_after :: Maybe Day
  , _c_before :: Maybe Day
  -- , _c_during :: [Either UTCTime UTCTime]
  }

data Query
  = Query_Day Day
  | Query_Like Text
  | Query_And (NonEmpty Text)

parseQuery :: Text -> Query
parseQuery query = case NEL.nonEmpty (T.words query) of
  Nothing -> Query_Like query
  Just qs -> Query_And qs

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
  Query_Like q -> filter_ $ msgContaining q
  Query_And qs -> filter_ $ \msg -> foldr1 (&&.) $ flip msgContaining msg <$> qs
  where
    msgContaining q msg = _messageText msg `like_` val_ ("%" <> q <> "%")
