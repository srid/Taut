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

import Data.Pagination

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
                      Route_Search -> fmap paginatedRouteValue <$> askRoute
                      _ -> pure $ constDyn ""
                    searchInputWidgetWithRoute query $ \q' -> Route_Search :/ (PaginatedRoute (1, q'))
              )
            ]

          divClass "ui segment" $ subRoute_ $ \case
            Route_Home -> el "p" $ do
              text "Welcome to Taut, the Slack archive viewer. This app is a work in progress. Meanwhile, "
              routeLink sampleMsgR $ text "start from 2019/3/27"
              text "?"
            Route_Search -> do
              r  :: Dynamic t (PaginatedRoute Text) <- askRoute
              elClass "h1" "ui header" $ do
                text "Messages matching: "
                dynText $ paginatedRouteValue <$> r
              msgsE <- getMessages r urlForBackendSearchMessages
              renderMessagesWithPagination r Route_Search msgsE
            Route_Messages -> do
              -- TODO: this is also paginated
              r :: Dynamic t (PaginatedRoute Day) <- askRoute
              let day = paginatedRouteValue <$> r
              elClass "h1" "ui header" $ do
                text "Archive for "
                dynText $ showDay <$> day
              dyn_ $ ffor day $ \d -> do
                routeLink (Route_Messages :/ mkPaginatedRouteAtPage1 (addDays (-1) d)) $ elClass "button" "ui button" $ text "Prev Day"
              dyn_ $ ffor day $ \d -> do
                routeLink (Route_Messages :/ mkPaginatedRouteAtPage1 (addDays 1 d)) $ elClass "button" "ui button" $ text "Next Day"
              msgsE <- getMessages r urlForBackendGetMessages
              renderMessagesWithPagination r Route_Messages msgsE
          divClass "ui bottom attached secondary segment" $ do
            elAttr "a" ("href" =: "https://github.com/srid/Taut") $ text "Powered by Haskell"
  }
  where
    -- TODO: This should point to the very first day in the archives.
    sampleMsgR = (Route_Messages :/ mkPaginatedRouteAtPage1 (fromGregorian 2019 3 27))
    -- FIXME: refactor after https://github.com/obsidiansystems/obelisk/pull/286#issuecomment-489265962
    urlForBackendGetMessages pd = "/" <> T.intercalate "/" (["get-messages"] <> encodeDay (paginatedRouteValue pd)) <> "?page" <> (("=" <>) $ T.pack $ show $ paginatedRoutePageIndex pd)
    urlForBackendSearchMessages pr = "/search-messages/" <> paginatedRouteValue pr <> "?page" <> (("=" <>) $ T.pack $ show $ paginatedRoutePageIndex pr)

    renderMessagesWithPagination r mkR msgsE = do
      let pgn = attachWithMaybe
                (\pr mm -> (\(_, pm) -> (paginatedRouteValue pr, pm)) <$> mm)
                (current r) msgsE
      let pgnW = widgetHold_ blank $ ffor pgn $ \(q, pm) ->
            paginationNav pm $ \p' -> mkR :/ (PaginatedRoute (p', q))
      pgnW >> renderMessages msgsE >> pgnW

    getMessages
      :: (Reflex t, MonadHold t m, PostBuild t m, DomBuilder t m, Prerender js t m)
      => Dynamic t r
      -> (r -> Text)
      -> m (Event t (Maybe ([User], Paginated Message)))
    getMessages r mkUrl = switchHold never <=< dyn $ ffor r $ \x -> do
      fmap switchDyn $ prerender (pure never) $ do
        pb <- getPostBuild
        getAndDecode $ mkUrl x <$ pb
    renderMessages msgsE =
      widgetHold_ (divClass "ui loading segment" blank) $ ffor msgsE $ divClass "ui segment" . \case
        Nothing -> text "Something went wrong"
        Just (users', pm)
          | msgs == [] -> text "No results"
          | otherwise -> divClass "ui comments" $ do
              let users = Map.fromList $ ffor users' $ \u -> (_userId u, _userName u)
              forM_ (filter (isJust . _messageChannelName) msgs) $ singleMessage users
          where
            msgs = paginatedItems pm
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
