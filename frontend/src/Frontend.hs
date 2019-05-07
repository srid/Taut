{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad
import Data.Bool (bool)
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
import Common.Slack.Types.Auth
import Obelisk.Generated.Static

import Frontend.Message
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
        divClass "ui attached segment" $ mdo
          let itemClass active = bool "item" "active item" <$> active
          divClass "ui pointing inverted menu" $ subRouteMenu
            [ ( This Route_Home
              , \isActive -> routeLinkClass (Route_Home :/ ()) (itemClass isActive) $ text "Home"
              )
            , ( This Route_Messages
              , \isActive -> routeLinkClass sampleMsgR (itemClass isActive) $ text "Archive"
              )
            , ( This Route_Search
              , \isActive -> divClass "right menu" $ do
                  widgetHold_ blank $ ffor userE $ \(SlackUser name _) ->
                    divClass "item" $ text $ "Welcome " <> name
                  elDynClass "div" (itemClass isActive) $ do
                    -- FIXME: On hard page refresh the query is not being set as initial value in input.
                    query <- fmap join $ subRoute $ \case
                      Route_Search -> fmap paginatedRouteValue <$> askRoute
                      _ -> pure $ constDyn ""
                    searchInputWidgetWithRoute query $ \q' -> Route_Search :/ (PaginatedRoute (1, q'))
              )
            ]
          userE :: Event t SlackUser <- divClass "ui segment" $ fmap switchDyn $ subRoute $ \case
            Route_Home -> el "p" $ do
              text "Welcome to Taut, the Slack archive viewer. Click 'Archive' or do a search."
              pure never
            Route_Search -> do
              r  :: Dynamic t (PaginatedRoute Text) <- askRoute
              elClass "h1" "ui header" $ do
                text "Messages matching: "
                dynText $ paginatedRouteValue <$> r
              resp <- getMessages r $ (BackendRoute_SearchMessages :/)
              widgetHold_ (divClass "ui loading segment" blank) $ ffor resp $ \case
                Nothing -> text "Something went wrong"
                Just (Left na) -> notAuthorizedWidget na
                Just (Right (_, v)) -> do
                  renderMessagesWithPagination r Route_Search v
              pure $ fmap fst $ filterRight $ fforMaybe resp id
            Route_Messages -> do
              r :: Dynamic t (PaginatedRoute Day) <- askRoute
              let day = paginatedRouteValue <$> r
              elClass "h1" "ui header" $ do
                text "Archive for "
                dynText $ showDay <$> day
              resp <- getMessages r $ (BackendRoute_GetMessages :/)
              widgetHold_ (divClass "ui loading segment" blank) $ ffor resp $ \case
                Nothing -> text "Something went wrong"
                Just (Left na) -> notAuthorizedWidget na
                Just (Right (_, v)) -> do
                  dyn_ $ ffor day $ \d -> do
                    routeLink (Route_Messages :/ mkPaginatedRouteAtPage1 (addDays (-1) d)) $ elClass "button" "ui button" $ text "Prev Day"
                  dyn_ $ ffor day $ \d -> do
                    routeLink (Route_Messages :/ mkPaginatedRouteAtPage1 (addDays 1 d)) $ elClass "button" "ui button" $ text "Next Day"
                  renderMessagesWithPagination r Route_Messages v
              pure $ fmap fst $ filterRight $ fforMaybe resp id
          divClass "ui bottom attached secondary segment" $ do
            elAttr "a" ("href" =: "https://github.com/srid/Taut") $ text "Powered by Haskell"
  }
  where
    -- TODO: This should point to the very first day in the archives.
    sampleMsgR = (Route_Messages :/ mkPaginatedRouteAtPage1 (fromGregorian 2019 3 27))

    notAuthorizedWidget :: DomBuilder t m => NotAuthorized -> m ()
    notAuthorizedWidget = \case
      NotAuthorized_RequireLogin grantHref -> divClass "ui segment" $ do
        el "p" $ text "You must login to Slack to access this page."
        slackLoginButton grantHref
      NotAuthorized_WrongTeam (SlackTeam teamId) grantHref -> divClass "ui segment" $ do
        el "p" $ text $ "Your team " <> teamId <> " does not match that of the archives. Please login to the correct team."
        slackLoginButton grantHref
      where
        slackLoginButton r = elAttr "a" ("href" =: r) $
          elAttr "img" ("src" =: "https://api.slack.com/img/sign_in_with_slack.png") blank

    renderMessagesWithPagination r mkR (us, pm) = do
      let pgnW = dyn_ $ ffor r $ \pr ->
            paginationNav pm $ \p' -> mkR :/ (PaginatedRoute (p', paginatedRouteValue pr))
      pgnW >> messageList (us, pm) >> pgnW

    showDay day = T.pack $ printf "%d-%02d-%02d" y m d
      where
        (y, m, d) = toGregorian day
