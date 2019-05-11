{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Backend.Search.Parser where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Void
import Text.Read (readMaybe)
import GHC.Generics

import Text.Megaparsec
import Text.Megaparsec.Char

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

-- FIXME: Won't include stuff like hyphens.
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
