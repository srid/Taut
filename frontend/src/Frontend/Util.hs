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
import qualified Data.Text as T
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
     ( DomBuilder t m
     , MonadSample t m
     )
  => Dynamic t Text
  -> m (Dynamic t Text, Event t ())
searchInputWidget dq = do
  q <- sample $ current dq
  ie <- inputElement $ (def :: InputElementConfig EventResult t (DomBuilderSpace m))
    & inputElementConfig_initialValue .~ q
    & inputElementConfig_setValue .~ updated dq
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("placeholder" =: "Search...")
  let submitEvt = keypress Enter ie
  pure (value ie, submitEvt)

searchInputWidgetWithRoute
  :: ( DomBuilder t m
     , SetRoute t (R route) m
     , PostBuild t m
     , MonadSample t m
     , RouteToUrl (R route) m
     )
  => Dynamic t Text
  -> (Text -> R route)
  -> m ()
searchInputWidgetWithRoute dq mkRoute = divClass "ui transparent icon inverted input" $ do
  (val, submit) <- searchInputWidget dq
  setRoute $ mkRoute <$> tag (current val) submit
  dyn_ $ ffor val $ \q' ->
    routeLink' (mkRoute q') "i" ("class" =: "search link icon") blank

paginationNav
  :: forall t m route.
     ( DomBuilder t m
     , RouteToUrl (R route) m
     , SetRoute t (R route) m
     )
  => Word
  -> Int
  -> (Word -> R route)
  -> m ()
paginationNav page cnt mkRoute = do
  divClass "ui message" $ do
    if page > 1
      then routeLink (mkRoute $ page - 1) $
        elClass "button" "ui button" $ text "Prev"
      else blank
    text "Page "
    text $ T.pack $ show page
    text " of "
    text $ T.pack $ show cnt  -- TODO: show number of pages not matches
    text " matches"
    routeLink (mkRoute $ 1 + page) $
      elClass "button" "ui button" $ text "Next"

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
