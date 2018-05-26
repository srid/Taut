{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core

import Static

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = el "title" $ text "Taut"
    body = do
      el "h1" $ text "Taut"
      el "p" $ text "Work in progress"
      -- elAttr "img" ("src" =: static @"obelisk.jpg") blank
