{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
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

messageList :: DomBuilder t m => Paginated Message -> m ()
messageList pm
  | msgs == [] = text "No results"
  | otherwise = divClass "ui comments" $ do
      forM_ (filter (isJust . _messageChannelName) msgs) $ singleMessage
  where
    msgs = paginatedItems pm

singleMessage :: DomBuilder t m => Message -> m ()
singleMessage msg = do
  let mts = formatSlackTimestamp (_messageTs msg)
  elAttr "div" ("class" =: "comment" <> "id" =: mts) $ do
    divClass "content" $ do
      elClass "a" "author" $ do
        text $ fromMaybe "?unknown?" $ _messageUserName msg
      divClass "metadata" $ do
        divClass "room" $
          text $ fromMaybe "Unknown Channel" $ fmap ("#" <>) $ _messageChannelName msg
        divClass "date" $ do
          case _messageChannelName msg of
            Nothing -> text $ T.pack $ show $ _messageTs msg
            Just ch -> do
              let rr = renderFrontendRoute enc $ FrontendRoute_Search :/ (PaginatedRoute (Right (_messageTs msg), "in:" <> ch))
              elAttr "a" ("href" =: rr) $ text $ T.pack $ show $ _messageTs msg
      elAttr "div" ("class" =: "text") $ do
        renderSlackMessage $ _messageText msg
  where
    Right (enc :: Encoder Identity Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName) = checkEncoder backendRouteEncoder

-- TODO: This is not perfect yet.
renderSlackMessage :: DomBuilder t m => Text -> m ()
renderSlackMessage s = renderChunks $ T.splitOn "\n" s'
  where
    s' = T.replace "&gt;" ">" s
    renderChunks [] = blank
    renderChunks chunk@(c:cs) = do
      -- Blockquote all chunks if >>> is prefix.
      if | T.isPrefixOf ">>>" c -> forM_ chunk $ el "blockquote" . text
         | T.isPrefixOf ">" c -> el "blockquote" (text c) >> renderChunks cs
         | otherwise -> el "p" (text c) >> renderChunks cs

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
