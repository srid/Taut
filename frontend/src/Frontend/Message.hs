{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Frontend.Message where

import Control.Monad
import Data.Bool (bool)
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Maybe
import Data.Monoid (First (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
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
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Maybe UTCTime
  -> Paginated Message
  -> m (Dynamic t (Maybe (Element EventResult GhcjsDomSpace t)))
messageList toHighlight pm
  | msgs == [] = text "No results" >> pure (constDyn Nothing)
  | otherwise = divClass "ui comments" $ do
      fmap (fmap getFirst . mconcat) $ forM (filter (isJust . _messageChannelName) msgs) $
        fmap (fmap First) . singleMessage toHighlight
  where
    msgs = paginatedItems pm

singleMessage
  :: ( DomBuilder t m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Maybe UTCTime
  -> Message
  -> m (Dynamic t (Maybe (Element EventResult GhcjsDomSpace t)))
  -- ^ The element that the caller should scroll to.
singleMessage toHighlight msg = do
  let mts = formatSlackTimestamp (_messageTs msg)
      highlight = toHighlight == Just (_messageTs msg)
  e <- if highlight
    then prerender (pure Nothing) $ do
      fmap (Just . fst) $ el' "div" blank
    else pure $ constDyn Nothing
  elAttr "div" ("class" =: "comment" <> "id" =: mts) $ bool id (divClass "ui piled black segment") highlight $ do
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
              let rr = FrontendRoute_Search :/ (PaginatedRoute (Right (_messageTs msg), "in:" <> ch))
              routeLink rr $ text $ T.pack $ show $ _messageTs msg
      elAttr "div" ("class" =: "text") $ do
        renderSlackMessage $ _messageText msg
  pure e

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
