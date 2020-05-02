{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Frontend.Message where

import Common.Route
import Common.Slack.Internal (formatSlackTimestamp)
import Common.Slack.Types
import Common.Types
import Control.Monad
import Data.Bool (bool)
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid (First (..))
import Data.Pagination
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Text.Printf (printf)

messageList ::
  ( DomBuilder t m,
    Prerender js t m,
    SetRoute t (R FrontendRoute) m,
    RouteToUrl (R FrontendRoute) m
  ) =>
  Maybe UTCTime ->
  Paginated Message ->
  -- | The element that the caller should scroll to.
  m (Dynamic t (Maybe (Element EventResult GhcjsDomSpace t)))
messageList toHighlight pm
  | msgs == [] = text "No results" >> pure (constDyn Nothing)
  | otherwise = divClass "ui comments" $ do
    fmap (fmap getFirst . mconcat) $ forM msgs $ \msg -> do
      let highlight = toHighlight == Just (_messageTs msg)
      bookmark <-
        if highlight
          then bookmarkElement
          else pure $ constDyn Nothing
      singleMessage highlight msg
      pure $ fmap First bookmark
  where
    msgs = filter hasChannel $ paginatedItems pm
    hasChannel = isJust . _messageChannelName
    bookmarkElement ::
      ( DomBuilder t m,
        Prerender js t m
      ) =>
      m (Dynamic t (Maybe (Element EventResult GhcjsDomSpace t)))
    bookmarkElement = prerender (pure Nothing) $ do
      fmap (Just . fst) $ el' "div" blank

singleMessage ::
  ( DomBuilder t m,
    SetRoute t (R FrontendRoute) m,
    RouteToUrl (R FrontendRoute) m,
    Prerender js t m
  ) =>
  Bool ->
  Message ->
  m ()
singleMessage highlight msg = do
  let mts = formatSlackTimestamp (_messageTs msg)
  elAttr "div" ("class" =: "comment" <> "id" =: mts) $ bool id (elAttr "div" ("style" =: "background-color: PapayaWhip;")) highlight $ do
    divClass "content" $ do
      elClass "a" "author" $ do
        text $ fromMaybe "?unknown?" $ _messageUserName msg
      divClass "metadata" $ do
        divClass "room"
          $ text
          $ fromMaybe "Unknown Channel"
          $ fmap ("#" <>)
          $ _messageChannelName msg
        divClass "date" $ do
          case _messageChannelName msg of
            Nothing -> text $ T.pack $ show $ _messageTs msg
            Just ch -> do
              routeLink (permalink ch) $ text $ T.pack $ show $ _messageTs msg
      elAttr "div" ("class" =: "text") $ do
        renderSlackMessage $ _messageText msg
  where
    -- A message permalink is a listing of all messages in its channel, with the
    -- cursor pointing to the message itself.
    permalink ch = FrontendRoute_Search :/ (PaginatedRoute (Right (_messageTs msg), "in:" <> ch))

-- TODO: This is not perfect yet.
renderSlackMessage :: DomBuilder t m => Text -> m ()
renderSlackMessage s = renderChunks $ T.splitOn "\n" s'
  where
    s' = T.replace "&gt;" ">" s
    renderChunks [] = blank
    renderChunks chunk@(c : cs) = do
      -- Blockquote all chunks if >>> is prefix.
      if  | T.isPrefixOf ">>>" c -> forM_ chunk $ el "blockquote" . text
          | T.isPrefixOf ">" c -> el "blockquote" (text c) >> renderChunks cs
          | otherwise -> el "p" (text c) >> renderChunks cs

getMessages ::
  (Reflex t, MonadHold t m, PostBuild t m, DomBuilder t m, Prerender js t m) =>
  Dynamic t r ->
  (r -> R BackendRoute) ->
  m (Event t (Maybe MessagesResponse))
getMessages dr mkUrl = switchHold never <=< dyn $ ffor dr $ \r -> do
  fmap switchDyn $ prerender (pure never) $ do
    pb <- getPostBuild
    getAndDecode $ (renderBackendRoute enc . mkUrl) r <$ pb
  where
    Right (enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName) = checkEncoder fullRouteEncoder

getSearchExamples ::
  (MonadHold t m, PostBuild t m, Prerender js t m) =>
  m (Event t (Maybe ExamplesResponse))
getSearchExamples = do
  fmap switchDyn $ prerender (pure never) $ do
    pb <- getPostBuild
    getAndDecode $ (renderBackendRoute enc $ (BackendRoute_GetSearchExamples :/ ())) <$ pb
  where
    Right (enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName) = checkEncoder fullRouteEncoder

routeForDay :: Day -> R FrontendRoute
routeForDay day = FrontendRoute_Search :/ mkPaginatedRouteAtPage1 ("during:" <> showDay day)

showDay :: Day -> Text
showDay day = T.pack $ printf "%d-%02d-%02d" y m d
  where
    (y, m, d) = toGregorian day
