{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Backend.Query where

import Control.Lens hiding ((<.))
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
  -- | SearchModifier_Before Day
  -- | SearchModifier_After Day
  | SearchModifier_HasPin
  -- | HasLink
  -- | HasEmoji EmojiCode
  -- | SearchModifier_During (Either Day (Either Int Int)  -- On day, month or year.
  deriving (Show, Generic)

newtype SearchKeyword = SearchKeyword { unSearchKeyword :: Text }
  deriving (Show, Generic)

type Parser = Parsec Void Text

searchModifier :: Parser SearchModifier
searchModifier =
      try (SearchModifier_From <$> attribute "from")
  <|> try (SearchModifier_In <$> attribute "in")
  <|> try (SearchModifier_HasPin <$ string "has:pin")

searchKeyword :: Parser SearchKeyword
searchKeyword = SearchKeyword . T.pack <$> some alphaNumChar  -- TODO: support quoted strings

attribute :: Text -> Parser Text
attribute name = do
  _ <- string name
  _ <- string ":"
  T.pack <$> some alphaNumChar

parseSearchQuery :: Text -> Either (ParseError Char Void) [Either SearchModifier SearchKeyword]
parseSearchQuery q = runParser p "<user-query>" q
  where
    p = sepBy (Left <$> try searchModifier <|> Right <$> searchKeyword) space1

data MessageFilters = MessageFilters
  { _messageFilters_terms :: [SearchKeyword]
  , _messageFilters_from :: [Text]
  , _messageFilters_in :: [Text]
  , _messageFilters_hasPin :: Maybe ()
  -- , _messageFilters_after :: Maybe Day
  -- , _c_before :: Maybe Day
  -- , _c_during :: [Either UTCTime UTCTime]
  }
  deriving (Show, Generic)

makeLenses ''MessageFilters

mkMessageFilters :: [Either SearchModifier SearchKeyword] -> MessageFilters
mkMessageFilters = foldl f ini
  where
    ini = MessageFilters mempty mempty mempty Nothing
    f mf = \case
      Right kw -> mf & messageFilters_terms %~ (kw:)
      Left md -> case md of
        SearchModifier_From user -> mf & messageFilters_from %~ (user:)
        SearchModifier_In chan -> mf & messageFilters_in %~ (chan:)
        SearchModifier_HasPin -> mf & messageFilters_hasPin ?~ ()

demo :: IO ()
demo = parseTest
  (sepBy (Left <$> try searchModifier <|> Right <$> searchKeyword) space1)
  "from:srid hello world  has:pin in:general more  query"


messageFilters
  :: forall be s.
     ( BeamSqlBackend be
     , BeamSqlBackendIsString be Text
     , HasSqlValueSyntax (BeamSqlBackendValueSyntax be) Text
     , HasSqlValueSyntax (BeamSqlBackendValueSyntax be) (Maybe Text)
     , HasSqlEqualityCheck be Text
     -- , HasSqlValueSyntax (BeamSqlBackendValueSyntax be) UTCTime
     )
  => MessageFilters
  -> Q be SlackDb s (MessageT (QExpr be s))
  -> Q be SlackDb s (MessageT (QExpr be s))
messageFilters mf = filter_ $ \msg -> foldr1 (&&.) $
  [ foldr1 (&&.) $ msgContaining msg <$> (unSearchKeyword <$> _messageFilters_terms mf) -- TODO: Use NonEmpty list
  , foldr1 (||.) $ msgFrom msg <$> _messageFilters_from mf
  ]
  where
    msgContaining msg q = _messageText msg `like_` val_ ("%" <> q <> "%")
    msgFrom msg user = _messageUser msg ==. val_ (Just user)
  -- Query_Day day ->
  --   let fromDate = UTCTime day 0
  --       toDate = UTCTime (addDays 1 day) 0
  --   in filter_ (\msg -> (_messageTs msg >=. val_ fromDate) &&. (_messageTs msg <. val_ toDate))
  -- Query_Like q -> filter_ $ msgContaining q
  -- Query_And qs -> filter_ $ \msg -> foldr1 (&&.) $ flip msgContaining msg <$> qs


-- OLD CODE BELOW

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
