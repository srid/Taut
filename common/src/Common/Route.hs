{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Route where

import Prelude hiding ((.))

import Control.Monad.Except
import Data.Functor.Sum
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Text.Read (readMaybe)

import Obelisk.Route
import Obelisk.Route.TH

--TODO: Should we rename `Route` to `AppRoute`?
data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  --TODO: How do we do routes with strongly-typed results?
  BackendRoute_GetMessages :: BackendRoute Day

data Route :: * -> * where
  Route_Home :: Route ()
  Route_Messages :: Route Day

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute Route))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_GetMessages -> PathSegment "get-messages" $ dayEncoder
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      Route_Home -> PathEnd $ unitEncoder mempty
      Route_Messages -> PathSegment "messages" $ dayEncoder

-- TODO: clean this up
dayEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse Day PageName
dayEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_decode = \(path, _query) -> case path of
      [y, m, d] -> maybe (throwError "dayEncoder: invalid day") pure $ parseDay y m d
      _ -> throwError "dayEncoder: expected exactly 3 path elements"
  , _encoderImpl_encode = \day -> (encodeDay day, mempty)
  }
  where
    parseDay y' m' d' = do
      y <- readMaybe $ T.unpack y'
      m <- readMaybe $ T.unpack m'
      d <- readMaybe $ T.unpack d'
      fromGregorianValid y m d

-- TODO: Make this generic at the routes level.
--
-- Specifically we need a function `R BackendRoute -> Text` but I know only of
-- `BackendRoute () -> Text`.
urlForBackendGetMessages :: Day -> Text
urlForBackendGetMessages day = T.intercalate "/" $ ["get-messages"] <> encodeDay day

encodeDay :: Day -> [Text]
encodeDay day = [T.pack $ show y, T.pack $ show m, T.pack $ show d]
  where
    (y, m, d) = toGregorian day


concat <$> mapM deriveRouteComponent
  [ ''Route
  , ''BackendRoute
  ]
