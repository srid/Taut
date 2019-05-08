{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend.Query where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Foldable (foldr1)
import Data.Time.Clock
import qualified Data.List.NonEmpty as NEL
import Data.Void
import Data.List.NonEmpty (NonEmpty (..))

import Database.Beam
import Database.Beam.Backend.SQL
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Common.Slack.Types

import Backend.Import (SlackDb (..))

data SearchQuery
  = SearchQuery_Like Text
  | SearchQuery_Or (NonEmpty SearchQuery)
  | SearchQuery_From Text
  | SearchQuery_To Text
  deriving (Show)

type Parser = Parsec Void Text

someTerm :: Parser Text
someTerm = fmap T.pack $ some alphaNumChar

searchQuery :: Parser SearchQuery
searchQuery = fmap combineOr p
  where
    p =
        (SearchQuery_From <$> (L.symbol space "from:" >> someTerm))
      <|> (SearchQuery_To <$> (L.symbol space "to:" >> someTerm))
      <|> (SearchQuery_Like . T.pack <$> some alphaNumChar) -- (sepEndBy1 printChar space1))
    combineOr = id
  -- <|> (SearchQuery_Or . fromJust . NEL.nonEmpty <$> sepEndBy1 searchQuery space1)

demo :: IO ()
demo = parseTest searchQuery "Hello world"

data C = C
  { _c_terms :: [Text]
  , _c_after :: Maybe UTCTime
  , _c_before :: Maybe UTCTime
  , _c_during :: Maybe (Either UTCTime UTCTime)
  , _c_from :: Maybe [Text]
  , _c_to :: Maybe [Text]
  }

-- TODO: Provide example queries for each constructor and display that in home
-- page. (Store queries in config file)
data Query
  = Query_Day Day
  | Query_Like Text
  | Query_And (NonEmpty Text)
  -- | Query_Exact Text
  -- | Query_Or Query Query
  -- | Query_And Query Query
  -- | Query_After UTCTime
  -- | Query_Between UTCTime
  -- | Query_During (Either Month Year)
  -- | Query_From SlackUser

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
