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
import qualified Data.Text as T
import Data.Time.Clock (utctDay)

import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Data.Pagination

import Common.Route
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
      let day = utctDay $ _messageTs msg
          r = FrontendRoute_Messages :/ mkPaginatedRouteAtPage1 day -- TODO: Determine page where message lies.
      divClass "metadata" $ do
        divClass "room" $ text $ fromMaybe "Unknown Channel" $ fmap ("#" <>) $ _messageChannelName msg
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
