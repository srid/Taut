{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T

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
      el "title" $ subRoute_ $ \case
        Route_Home -> text "Taut"
        Route_Messages -> do
          r :: Dynamic t (Int, Int, Int) <- askRoute
          dynText $ fmap (T.pack . show) r
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"semantic.min.css") blank
  , _frontend_body = do
      divClass "ui container" $ do
        divClass "ui top attached inverted header" $ do
          text "Taut"
        divClass "ui attached segment" $ subRoute_ $ \case
          Route_Home -> el "p" $ text "We'll show your Slack archive here. Hold tight!"
          Route_Messages -> do
            r :: Dynamic t (Int, Int, Int) <- askRoute
            el "p" $ do
              text "Messages for: "
              dynText $ fmap (T.pack . show) r
        divClass "ui bottom attached secondary segment" $ do
          el "p" $ text "This is a work in progress"
  , _frontend_notFoundRoute = \_ -> Route_Home :/ () -- TODO: not used i think
  }
