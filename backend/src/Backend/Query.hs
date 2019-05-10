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
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
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

allMessages :: MessageFilters
allMessages = MessageFilters [] [] [] Nothing

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
     , HasSqlEqualityCheck be Text
     -- , HasSqlValueSyntax (BeamSqlBackendValueSyntax be) UTCTime
     )
  => MessageFilters
  -> Q be SlackDb s (MessageT (QExpr be s))
  -> Q be SlackDb s (MessageT (QExpr be s))
messageFilters mf = filter_ $ \msg -> foldl (&&.) (val_ True) $ catMaybes $
  [ maybe Nothing (Just . foldl1 (&&.)) $ NEL.nonEmpty $
      msgContaining msg . unSearchKeyword <$> mf ^. messageFilters_terms
  , maybe Nothing (Just . foldl1 (||.)) $ NEL.nonEmpty $
      msgFrom msg <$> mf ^. messageFilters_from
  ]
  where
    msgContaining msg q = _messageText msg `like_` val_ ("%" <> q <> "%")
    msgFrom msg (user :: Text) = _messageUserName msg ==. just_ (val_ user) -- FIXME: join with userName
