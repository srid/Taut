{-# LANGUAGE ScopedTypeVariables #-}
module Common.Slack.Internal where

import Data.Aeson
import Data.Char (isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Text.Casing (fromHumps, toQuietSnake)
import Text.Read (readMaybe)

-- | Field label modifier for converting Slack datatypes to json
fieldLabelMod :: Options
fieldLabelMod = defaultOptions
  { fieldLabelModifier = toQuietSnake . fromHumps . dropWhile (not . isUpper)
  }

parseSlackTimestamp :: Applicative f => Text -> f UTCTime
parseSlackTimestamp ts' = case readMaybe (T.unpack ts') of
  Nothing -> error $ "Invalid ts: " <> T.unpack ts'
  Just (ts :: Double) -> do
    -- FIXME: Not sure if `round` is problematic here. We want this value to be unique.
    pure $ posixSecondsToUTCTime $ fromInteger $ round ts

-- TODO: Check if this converts back to the same value
-- NOTE: it does not; loses the double precision. do we care?
formatSlackTimestamp :: UTCTime -> Text
formatSlackTimestamp t = T.pack $ show $ (realToFrac (utcTimeToPOSIXSeconds t) :: Double)
