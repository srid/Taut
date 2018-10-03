{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad
import Data.Foldable (foldl')
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Monoid hiding (Sum, (<>))
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Text.Printf (printf)

import Reflex.Dom.Core

import Obelisk.Frontend
import Obelisk.Route.Frontend

import Static

import Common.Route
import Common.Slack.Types

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
              selectedDate <- fmap value $ inputElement $ def
                & initialAttributes .~ ("type" =: "date")
                & inputElementConfig_initialValue .~ toDateInputVal day
              tellEvent $ ffor (updated selectedDate) $ \v ->
                Endo $ const $ Route_Messages :/ parseDateInput v
              el "h1" $ do
                text "Messages for: "
                text $ toDateInputVal day
              pb <- getPostBuild
              v' :: Event t (Maybe ([User], [Message])) <- prerender (pure never) $
                getAndDecode $ urlForBackendGetMessages day <$ pb
              widgetHold_ (text "Loading") $ ffor v' $ \case
                Nothing -> text "No data"
                Just (users', msgs) -> divClass "ui comments" $ do
                  let users = Map.fromList $ ffor users' $ \u -> (_userId u, _userName u)
                  forM_ (filter (isJust . _messageChannelName) msgs) $ singleMessage users
        divClass "ui bottom attached secondary segment" $ do
          el "p" $ text "This is a work in progress"
  , _frontend_notFoundRoute = \_ -> Route_Home :/ () -- TODO: not used i think
  }
  where
    toDateInputVal day = T.pack $ printf "%d-%02d-%02d" y m d
      where
        (y, m, d) = toGregorian day
    parseDateInput val =
      fromGregorian (read $ T.unpack y) (read $ T.unpack m) (read $ T.unpack d)
      where
        [y, m, d] = T.splitOn "-" val

singleMessage :: DomBuilder t m => Map.Map Text Text -> Message -> m ()
singleMessage users msg = do
  divClass "comment" $ do
    divClass "content" $ do
      elClass "a" "author" $ text $ maybe "Nobody" (\u -> Map.findWithDefault "Unknown" u users) $ _messageUser msg
      divClass "metadata" $ do
        divClass "date" $ text $ T.pack $ show $ _messageTs msg
      elAttr "div" ("class" =: "text" <> "title" =: T.pack (show msg)) $ do
        text $ renderText $ _messageText msg
  where
    renderText s = foldl' (\m (userId, userName) -> T.replace userId userName m) s (Map.toList users)
