{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad
import Data.Bool (bool)
import Data.Foldable (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Semigroup ((<>))
import Data.Some
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Text.Printf (printf)

import Reflex.Dom.Core

import Obelisk.Frontend
import Obelisk.Route.Frontend

import Common.Route
import Common.Slack.Types
import Obelisk.Generated.Static

import Frontend.Util

frontend :: Frontend (R Route)
frontend = Frontend
  { _frontend_head = do
      elAttr "base" ("href" =: "/") blank
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
      el "title" $ text "Taut - Slack Archives"
      -- FIXME: This throws JSException
      -- el "title" $ subRoute_ $ \case
      --   Route_Home -> text "Taut"
      --   Route_Messages -> do
      --     r :: Dynamic t Day <- askRoute
      --     text "Taut - "
      --     dynText $ fmap (T.pack . show) r
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"semantic.min.css") blank
  , _frontend_body = do
      divClass "ui container" $ do
        divClass "ui attached segment" $ do
          let itemClass active = bool "item" "active item" <$> active
          divClass "ui pointing inverted menu" $ subRouteMenu
            [ ( This Route_Home
              , \isActive -> routeLinkClass (Route_Home :/ ()) (itemClass isActive) $ text "Home"
              )
            , ( This Route_Messages
              , \isActive -> routeLinkClass sampleMsgR (itemClass isActive) $ text "Archive"
              )
            , ( This Route_Search
              , \isActive -> divClass "right menu" $
                  elDynClass "div" (itemClass isActive) $ do
                    -- FIXME: On hard page refresh the query is not being set as initial value in input.
                    query <- fmap join $ subRoute $ \case
                      Route_Search -> fmap fst <$> askRoute
                      _ -> pure $ constDyn ""
                    searchInputWidgetWithRoute query $ \q' -> Route_Search :/ (q', Nothing)
              )
            ]

          divClass "ui segment" $ subRoute_ $ \case
            Route_Home -> el "p" $ do
              text "Welcome to Taut, the Slack archive viewer. This app is a work in progress. Meanwhile, "
              routeLink sampleMsgR $ text "start from 2019/3/27"
              text "?"
            Route_Search -> do
              queryWithPage  :: Dynamic t (Text, Maybe Word) <- askRoute
              elClass "h1" "ui header" $ do
                text "Search results for "
                dynText $ T.pack . show <$> queryWithPage
              let mpage :: Dynamic t (Maybe Word) = maybe (Just 1) Just . snd <$> queryWithPage
              msgsE <- getMessages queryWithPage
                -- FIXME: refactor after https://github.com/obsidiansystems/obelisk/pull/286#issuecomment-489265962
                (\(q, p) -> "/search-messages/" <> q <> "?page" <> (maybe "" ("=" <>) $ T.pack . show <$> p))
              widgetHold_ blank $ ffor msgsE $ \case
                Nothing -> blank
                Just (_, _, cnt) -> dyn_ $ ffor mpage $ maybe blank $ \page ->
                  divClass "ui message" $ do
                    dyn_ $ ffor queryWithPage $ \(q, p') -> do
                      let p = fromMaybe 1 p'
                      if p > 1
                        then routeLink (Route_Search :/ (q, Just $ p - 1)) $
                          elClass "button" "ui button" $ text "Prev"
                        else blank
                    text "Page "
                    text $ T.pack $ show page
                    text " of "
                    text $ T.pack $ show cnt
                    text " matches"
                    dyn_ $ ffor queryWithPage $ \(q, p) ->
                      routeLink (Route_Search :/ (q, Just $ 1 + fromMaybe 1 p)) $
                        elClass "button" "ui button" $ text "Next"
              renderMessages msgsE
            Route_Messages -> do
              r :: Dynamic t Day <- askRoute
              elClass "h1" "ui header" $ do
                text "Archive for "
                dynText $ showDay <$> r
              dyn_ $ ffor r $ \day -> do
                routeLink (Route_Messages :/ addDays (-1) day) $ elClass "button" "ui button" $ text "Prev Day"
              dyn_ $ ffor r $ \day -> do
                routeLink (Route_Messages :/ addDays 1 day) $ elClass "button" "ui button" $ text "Next Day"
              msgsE <- getMessages r urlForBackendGetMessages
              renderMessages msgsE
          divClass "ui bottom attached secondary segment" $ do
            elAttr "a" ("href" =: "https://github.com/srid/Taut") $ text "Powered by Haskell"
  }
  where
    -- TODO: This should point to the very first day in the archives.
    sampleMsgR = (Route_Messages :/ fromGregorian 2019 3 27)

    getMessages
      :: (Reflex t, MonadHold t m, PostBuild t m, DomBuilder t m, Prerender js t m)
      => Dynamic t r -> (r -> Text) -> m (Event t (Maybe ([User], [Message], Int)))
    getMessages r mkUrl = switchHold never <=< dyn $ ffor r $ \x -> do
      fmap switchDyn $ prerender (pure never) $ do
        pb <- getPostBuild
        getAndDecode $ mkUrl x <$ pb
    renderMessages msgsE =
      widgetHold_ (divClass "ui loading segment" blank) $ ffor msgsE $ divClass "ui segment" . \case
        Nothing -> text "Something went wrong"
        Just (users', msgs, _cnt)
          | msgs == [] -> text "No results"
          | otherwise -> divClass "ui comments" $ do

              let users = Map.fromList $ ffor users' $ \u -> (_userId u, _userName u)
              forM_ (filter (isJust . _messageChannelName) msgs) $ singleMessage users
    showDay day = T.pack $ printf "%d-%02d-%02d" y m d
      where
        (y, m, d) = toGregorian day

singleMessage :: DomBuilder t m => Map.Map Text Text -> Message -> m ()
singleMessage users msg = do
  divClass "comment" $ do
    divClass "content" $ do
      elClass "a" "author" $ do
        text $ maybe "Nobody" (\u -> Map.findWithDefault "Unknown" u users) $ _messageUser msg
      divClass "metadata" $ do
        divClass "room" $ text $ fromMaybe "Unknown Channel" $ fmap ("#" <>) $ _messageChannelName msg
        divClass "date" $ text $ T.pack $ show $ _messageTs msg
      elAttr "div" ("class" =: "text" <> "title" =: T.pack (show msg)) $ do
        text $ renderText $ _messageText msg
  where
    renderText s = foldl' (\m (userId, userName) -> T.replace userId userName m) s (Map.toList users)
