{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Reflex.Dom.Core

import Obelisk.Frontend
import Obelisk.Route.Frontend

import Common.Route

frontend :: Frontend (R Route)
frontend = Frontend
  { _frontend_head = subRoute_ $ \_r -> do
      el "title" $ text "Taut"
  , _frontend_body = subRoute_ $ \_r -> do
      el "h1" $ text "Taut"
      el "p" $ text "Work in progress"
  , _frontend_notFoundRoute = \_ -> Route_Home :/ () -- TODO: not used i think
  }
