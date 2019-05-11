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

import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Time.Calendar

import Reflex.Dom.Core

import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route.Frontend

import Common.Route
import Common.Slack.Types.Auth
import qualified Common.Slack.Types.Search as Search

import Frontend.Message
import Frontend.Util


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      elAttr "base" ("href" =: "/") blank
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
      el "title" $ text "Taut - Slack Archive Viewer"
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
        let color = "teal"
        divClass ("ui top attached inverted segment " <> color) $
          elClass "h1" "ui header" $
            routeLink (FrontendRoute_Home :/ ()) $ text "Taut - Slack Archive Viewer"
        divClass "ui attached segment" $ mdo
          divClass ("ui raised segment " <> color) $ divClass "ui icon inverted fluid input" $ do
            query <- holdDyn "" $ fmap fst userE
            searchInputWidgetWithRoute query $ \q ->
              FrontendRoute_Search :/ (PaginatedRoute (1, q))
          userE :: Event t (Text, SlackUser) <- fmap switchDyn $ subRoute $ \case
            FrontendRoute_Home ->
              pure never
            FrontendRoute_Search -> do
              r  :: Dynamic t (PaginatedRoute Text) <- askRoute
              resp <- getMessages r $ (BackendRoute_SearchMessages :/)
              widgetHold_ (divClass "ui loading segment" blank) $ ffor resp $ \case
                Nothing -> text "Something went wrong"
                Just (Left na) -> notAuthorizedWidget na
                Just (Right (_, (mf, v))) -> do
                  case Search.isOnlyDuring mf of
                    Nothing -> do
                      elClass "h2" "ui header" $ do
                        text "Messages matching: "
                        dynText $ paginatedRouteValue <$> r
                    Just day -> do
                      elClass "h2" "ui header" $ do
                        text $ "Messages on day: " <> showDay day
                      routeLink (routeForDay $ addDays (-1) day) $ elClass "button" "ui button" $ text "Prev Day"
                      routeLink (routeForDay $ addDays 1 day) $ elClass "button" "ui button" $ text "Next Day"
                  renderMessagesWithPagination r FrontendRoute_Search v
              pure $ attachWith (,) (current $ fmap paginatedRouteValue r) $ fmap fst $ filterRight $ fforMaybe resp id
          divClass "ui bottom attached secondary segment" $ do
            elAttr "a" ("href" =: "https://github.com/srid/Taut") $ text "Powered by Haskell"
            widgetHold_ blank $ ffor userE $ \(_, SlackUser name _) ->
              divClass "item" $ text $ "Welcome " <> name
  }
  where
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

    renderMessagesWithPagination r mkR pm = do
      let pgnW = dyn_ $ ffor r $ \pr ->
            paginationNav pm $ \p' -> mkR :/ (PaginatedRoute (p', paginatedRouteValue pr))
      pgnW >> messageList pm >> pgnW
