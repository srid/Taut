module Main where

import Data.Monoid ((<>))
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Taut.Slack.Types

rootDir :: String
rootDir = "/home/srid/tmp/data"

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
