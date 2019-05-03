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
import Control.Monad
import Data.GADT.Compare (GEq)
import Data.Map (Map)
import Data.Proxy
import Data.Some
import Data.Text (Text)
import Prelude

import Obelisk.Route.Frontend
import Reflex.Dom

subRouteMenu
  :: (GEq r, DomBuilder t m)
  => [(Some r, (Dynamic t Bool -> RoutedT t (R r) m ()))]
  -> RoutedT t (R r) m ()
subRouteMenu rs = do
  forM_ rs $ \(mr, w) -> do
    r <- askRoute
    let active = ffor r $ \(r' :/ _) -> mr == This r'
    w active

searchInputWidget
  :: forall t m.
     DomBuilder t m
  => Text
  -> m (Dynamic t Text, Event t ())
searchInputWidget q = do
  ie <- inputElement $ (def :: InputElementConfig EventResult t (DomBuilderSpace m))
    & inputElementConfig_initialValue .~ q
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("placeholder" =: "Search...")
  let submitEvt = keypress Enter ie
  pure (value ie, submitEvt)

searchInputWidgetWithRoute
  :: ( DomBuilder t m
     , SetRoute t (R route) m
     , PostBuild t m, RouteToUrl (R route) m
     )
  => Text
  -> (Text -> R route)
  -> m ()
searchInputWidgetWithRoute q mkRoute = divClass "ui transparent icon inverted input" $ do
  (val, submit) <- searchInputWidget q
  setRoute $ mkRoute <$> tag (current val) submit
  dyn_ $ ffor val $ \q' ->
    routeLink' (mkRoute q') "i" ("class" =: "search link icon") blank

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
