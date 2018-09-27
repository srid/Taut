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
      BackendRoute_GetPage -> pathOnlyValidEncoder
    InR obeliskRoute -> runValidEncoderFunc myObeliskRestValidEncoder obeliskRoute

--TODO: Should we rename `Route` to `AppRoute`?
data BackendRoute :: * -> * where
  --TODO: How do we do routes with strongly-typed results?
  BackendRoute_GetPage :: BackendRoute [Text]

data Route :: * -> * where
  Route_Home :: Route ()
  Route_Messages :: Route Day

backendRouteComponentEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (Some BackendRoute) (Maybe Text)
backendRouteComponentEncoder = enumEncoder $ \case
  Some.This BackendRoute_GetPage -> Just "get-page"

backendRouteRestEncoder :: (Applicative check, MonadError Text parse) => BackendRoute a -> Encoder check parse a PageName
backendRouteRestEncoder = Encoder . pure . \case
  BackendRoute_GetPage -> pathOnlyValidEncoder

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
  Route_Messages -> dateEncoder

dateEncoder :: MonadError Text parse => ValidEncoder parse Day PageName
dateEncoder = ValidEncoder
  { _validEncoder_decode = \(path, query) ->
      if query == mempty
      then case path of
        [v] -> pure $ urlToDate v
        _ -> throwError "dateEncoder: expected exactly one path element"
      else throwError "dateEncoder: query was provided"
  , _validEncoder_encode = \day -> ([dateToUrl day], mempty)
  }
  where
    -- TODO: throwError on invalid input
    dateToUrl day = T.pack $ show y <> "-" <> show m <> "-" <> show d where (y, m, d) = toGregorian day
    urlToDate s = fromGregorian (read $ T.unpack y) (read $ T.unpack m) (read $ T.unpack d) where [y, m, d] = T.splitOn "-" s -- TODO: use fromGregorianValid


concat <$> mapM deriveRouteComponent
  [ ''Route
  , ''BackendRoute
  ]
