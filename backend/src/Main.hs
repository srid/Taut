{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Monoid ((<>))

import Database.Beam

import Taut.Slack.Types

rootDir :: String
rootDir = "/home/srid/tmp/data"

data SlackDb f = SlackDb
  { _slackDb_channels :: f (TableEntity ChannelT)
  }
  deriving (Generic)

instance Database SlackDb

slackDb :: DatabaseSettings be SlackDb
slackDb = defaultDbSettings

loadFile :: FromJSON a => String -> IO (Either String a)
loadFile path = B.readFile (rootDir <> path) >>= return . eitherDecode

loadUsers :: IO (Either String [User])
loadUsers = do
  usersData <- B.readFile "/home/srid/tmp/data/users.json"
  return $ (eitherDecode usersData :: Either String [User])

main :: IO ()
main = do
  -- (loadFile "/channels.json" :: IO (Either String [Channel])) >>= putStrLn . show
  (loadFile "/general/2018-02-15.json" :: IO (Either String [Message])) >>= putStrLn . show
