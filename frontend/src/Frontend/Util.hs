{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Frontend.Util where

import Control.Lens hiding (Bifunctor, bimap, element, universe)
import Data.Proxy
import Prelude hiding (id, (.))
import Data.Text (Text)
import Data.Map (Map)

import Obelisk.Route.Frontend
import Reflex.Dom

routeLinkClass
  :: forall t m a route.
     ( DomBuilder t m
     , RouteToUrl (R route) m
     , SetRoute t (R route) m
     , PostBuild t m
     )
  => R route -- ^ Target route
  -> Dynamic t Text -- ^ Class name
  -> m a -- ^ Child widget
  -> m ()
routeLinkClass r cls w = dyn_ $ ffor cls $ \c ->
  routeLink' r "a" ("class" =: c) w

routeLink'
  :: forall t m a route.
     ( DomBuilder t m
     , RouteToUrl (R route) m
     , SetRoute t (R route) m
     )
  => R route -- ^ Target route
  -> Text
  -> Map AttributeName Text
  -> m a -- ^ Child widget
  -> m a
routeLink' r elementTag attr w = do
  enc <- askRouteToUrl
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_initialAttributes .~ (attr <> "href" =: enc r)
  (e, a) <- element elementTag cfg w
  setRoute $ r <$ domEvent Click e
  return a
