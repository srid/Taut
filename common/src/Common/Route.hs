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

import Control.Category
import Control.Monad.Except
import Data.Functor.Sum
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Text.Read (readMaybe)

import Obelisk.Route
import Obelisk.Route.TH

backendRouteEncoder
  :: ( check ~ parse
     , MonadError Text parse
     )
  => Encoder check parse (R (Sum BackendRoute (ObeliskRoute Route))) PageName
backendRouteEncoder = Encoder $ do
  let myComponentEncoder = (backendRouteComponentEncoder `shadowEncoder` obeliskRouteComponentEncoder routeComponentEncoder) . someSumEncoder
  myObeliskRestValidEncoder <- checkObeliskRouteRestEncoder routeRestEncoder
  checkEncoder $ pathComponentEncoder myComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_GetMessages -> dayEncoder
    InR obeliskRoute -> runValidEncoderFunc myObeliskRestValidEncoder obeliskRoute

--TODO: Should we rename `Route` to `AppRoute`?
data BackendRoute :: * -> * where
  --TODO: How do we do routes with strongly-typed results?
  BackendRoute_GetMessages :: BackendRoute Day

data Route :: * -> * where
  Route_Home :: Route ()
  Route_Messages :: Route Day

backendRouteComponentEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (Some BackendRoute) (Maybe Text)
backendRouteComponentEncoder = enumEncoder $ \case
  Some.This BackendRoute_GetMessages -> Just "get-messages"

backendRouteRestEncoder :: (Applicative check, MonadError Text parse) => BackendRoute a -> Encoder check parse a PageName
backendRouteRestEncoder = Encoder . pure . \case
  BackendRoute_GetMessages -> dayEncoder

routeComponentEncoder
  :: (MonadError Text check, MonadError Text parse)
  => Encoder check parse (Some Route) (Maybe Text)
routeComponentEncoder = enumEncoder $ \case
  Some.This Route_Home -> Nothing
  Some.This Route_Messages -> Just "messages"

routeRestEncoder
  :: forall check parse a. (MonadError Text check, MonadError Text parse)
  => Route a -> Encoder check parse a PageName
routeRestEncoder = Encoder . pure . \case
  Route_Home -> endValidEncoder mempty
  Route_Messages -> dayEncoder

dayEncoder :: MonadError Text parse => ValidEncoder parse Day PageName
dayEncoder = ValidEncoder
  { _validEncoder_decode = \(path, _query) -> case path of
      [y, m, d] -> maybe (throwError "dayEncoder: invalid day") pure $ parseDay y m d
      _ -> throwError "dayEncoder: expected exactly 3 path elements"
  , _validEncoder_encode = \day -> (encodeDay day, mempty)
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
