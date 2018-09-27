{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Data.Time.Calendar

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
          r :: Dynamic t Day <- askRoute
          text "Taut - "
          dynText $ fmap (T.pack . show) r
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"semantic.min.css") blank
  , _frontend_body = do
      divClass "ui container" $ do
        divClass "ui top attached inverted header" $ do
          text "Taut"
        divClass "ui attached segment" $ subRoute_ $ \case
          Route_Home -> el "p" $ text "We'll show your Slack archive here. Hold tight!"
          Route_Messages -> do
            r :: Dynamic t Day <- askRoute
            dyn_ $ ffor r $ \day -> do
              el "p" $ do
                text "Messages for: "
                text $ T.pack $ show day
              pb <- getPostBuild
              -- TODO: don't hardcode url?
              v' :: Event t (Maybe Int) <- prerender (pure never) $ 
                getAndDecode $ urlForBackendGetMessages (fromGregorian 2017 4 7) <$ pb
              widgetHold_ (text "Loading") $ ffor v' $ \v -> text $ "We received: " <> T.pack (show v)
        divClass "ui bottom attached secondary segment" $ do
          el "p" $ text "This is a work in progress"
  , _frontend_notFoundRoute = \_ -> Route_Home :/ () -- TODO: not used i think
  }
