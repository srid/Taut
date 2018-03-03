module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Taut.Slack.Types

loadUsers :: IO (Either String [User])
loadUsers = do
  usersData <- B.readFile "/home/srid/tmp/data/users.json"

  return $ (eitherDecode usersData :: Either String [User])
main :: IO ()
main = do
  loadUsers >>= putStrLn . show
