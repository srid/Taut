{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Reflex.Dom.Core

import Obelisk.Frontend
import Obelisk.Route.Frontend

import Static

import Common.Route

frontend :: Frontend (R Route)
frontend = Frontend
  { _frontend_head = do
      elAttr "base" ("href" =: "/") blank
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
      el "title" $ do
        r <- askRoute
        dynText $ ffor r $ \case
          (_ :: R Route) -> "Taut" -- Placeholder for title changing logic
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"semantic.min.css") blank
  , _frontend_body = subRoute_ $ \_r -> do
      divClass "ui container" $ do
        divClass "ui top attached inverted header" $ do
          text "Taut"
        divClass "ui attached segment" $ do
          el "p" $ text "We'll show your Slack archive here. Hold tight!"
        divClass "ui bottom attached secondary segment" $ do
          el "p" $ text "This is a work in progress"
  , _frontend_notFoundRoute = \_ -> Route_Home :/ () -- TODO: not used i think
  }
