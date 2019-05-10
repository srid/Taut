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
import Control.Monad
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Void
import Text.Read (readMaybe)

import Database.Beam
import Database.Beam.Backend.SQL
import Text.Megaparsec
import Text.Megaparsec.Char
-- import qualified Text.Megaparsec.Char.Lexer as L

import Common.Slack.Types

import Backend.Import (SlackDb (..))

-- Learnings and observations:
-- * Megaparsec tutorial is long. Summarize key points to know?
--   * For someone that already is basically familiar with parser combinators
-- * Testing using hspec-megaparsec
-- * Importance of `try`

data SearchModifier
  = SearchModifier_From Text  -- ^ Sent by person
  | SearchModifier_In Text  -- ^ In channel
  | SearchModifier_During Day
  -- | SearchModifier_Before Day
  -- | SearchModifier_After Day
  | SearchModifier_HasPin
  -- | HasLink
  -- | HasEmoji EmojiCode
  deriving (Show, Generic)

newtype SearchKeyword = SearchKeyword { unSearchKeyword :: Text }
  deriving (Show, Generic)

type Parser = Parsec Void Text

searchModifier :: Parser SearchModifier
searchModifier =
      try (SearchModifier_From <$> attribute "from")
  <|> try (SearchModifier_In <$> attribute "in")
  <|>     (SearchModifier_During <$> dayParser)
  <|> try (SearchModifier_HasPin <$ string "has:pin")

dayParser :: Parser Day
dayParser = do
  let parseNum :: Read a => String -> Parser a
      parseNum e = maybe (fail e :: Parser a) pure =<< fmap readMaybe (some digitChar)
  void $ try $ string "during:"
  year :: Integer <- parseNum "Expected year"
  _ <- string "-"
  month :: Int <- parseNum "Expected month"
  _ <- string "-"
  d :: Int <- parseNum "Expected day"
  maybe (fail "Invalid day") pure $ fromGregorianValid year month d

searchKeyword :: Parser SearchKeyword
searchKeyword =
      (SearchKeyword <$> quotedText)
  <|> (SearchKeyword <$> someWord)

quotedText :: Parser Text
quotedText = do
  void $ try (string "\"")
  s <- T.pack <$> some (satisfy (/= '"'))
  void $ string "\""
  pure s

someWord :: Parser Text
someWord = T.pack <$> some alphaNumChar -- TODO: Review the accuracy of this

attribute :: Text -> Parser Text
attribute name = do
  void $ string $ name <> ":"
  T.pack <$> some alphaNumChar

parseSearchQuery :: Text -> Either (ParseError Char Void) [Either SearchModifier SearchKeyword]
parseSearchQuery q = runParser p "<user-query>" q
  where
    p = sepBy (Left <$> searchModifier <|> Right <$> searchKeyword) space1


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
