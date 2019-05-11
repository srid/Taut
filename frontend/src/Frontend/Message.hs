{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Frontend.Message where

import Control.Monad
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Text.Printf (printf)

import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Data.Pagination

import Common.Route
import Common.Slack.Internal (formatSlackTimestamp)
import Common.Slack.Types
import Common.Types

messageList
  :: ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Paginated Message -> m ()
messageList pm
  | msgs == [] = text "No results"
  | otherwise = divClass "ui comments" $ do
      forM_ (filter (isJust . _messageChannelName) msgs) $ singleMessage
  where
    msgs = paginatedItems pm

singleMessage
  :: ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Message -> m ()
singleMessage msg = do
  divClass "comment" $ do
    divClass "content" $ do
      elClass "a" "author" $ do
        text $ fromMaybe "?unknown?" $ _messageUserName msg
      let r = FrontendRoute_Search :/ (mkPaginatedRouteAtPage1 $ "at:" <> formatSlackTimestamp (_messageTs msg))
      divClass "metadata" $ do
        divClass "room" $
          text $ fromMaybe "Unknown Channel" $ fmap ("#" <>) $ _messageChannelName msg
        divClass "date" $ do
          routeLink r $ text $ T.pack $ show $ _messageTs msg
      elAttr "div" ("class" =: "text") $ do
        text $ _messageText msg

getMessages
  :: (Reflex t, MonadHold t m, PostBuild t m, DomBuilder t m, Prerender js t m)
  => Dynamic t r
  -> (r -> R BackendRoute)
  -> m (Event t (Maybe MessagesResponse))
getMessages dr mkUrl = switchHold never <=< dyn $ ffor dr $ \r -> do
  fmap switchDyn $ prerender (pure never) $ do
    pb <- getPostBuild
    getAndDecode $ (renderBackendRoute enc . mkUrl) r <$ pb
  where
    Right (enc :: Encoder Identity Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName) = checkEncoder backendRouteEncoder

getSearchExamples
  :: (MonadHold t m, PostBuild t m, Prerender js t m)
  => m (Event t (Maybe ExamplesResponse))
getSearchExamples = do
  fmap switchDyn $ prerender (pure never) $ do
    pb <- getPostBuild
    getAndDecode $ (renderBackendRoute enc $ (BackendRoute_GetSearchExamples :/ ())) <$ pb
  where
    Right (enc :: Encoder Identity Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName) = checkEncoder backendRouteEncoder

routeForDay :: Day -> R FrontendRoute
routeForDay day = FrontendRoute_Search :/ mkPaginatedRouteAtPage1 ("during:" <> showDay day)

showDay :: Day -> Text
showDay day = T.pack $ printf "%d-%02d-%02d" y m d
  where
    (y, m, d) = toGregorian day
