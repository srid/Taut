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
import Data.Maybe (fromMaybe, isJust)
import Data.Semigroup ((<>))
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
        divClass "ui top attached inverted header" $ do
          text "Taut"
        divClass "ui attached segment" $ do
          -- TODO: clean up the tab widget
          divClass "ui pointing menu" $ do
            homeCls :: Dynamic t Text <- subRoute $ \case
              Route_Home -> pure "active item"
              _ -> pure "item"
            msgCls :: Dynamic t Text <- subRoute $ \case
              Route_Messages -> pure "active item"
              _ -> pure "item"
            searchCls :: Dynamic t Text <- subRoute $ \case
              Route_Search -> pure "active item"
              _ -> pure "item"
            routeLinkClass (Route_Home :/ ()) homeCls $ text "Home"
            elDynClass "a" msgCls $ text "Archive"
            divClass "right menu" $
              elDynClass "div" searchCls $ do
                divClass "ui transparent icon input" $ do
                  searchQuery :: Dynamic t Text <- fmap join $ subRoute $ \case
                    Route_Search -> askRoute
                    _ -> pure $ constDyn ""
                  dyn_ $ ffor searchQuery $ \q -> do
                    -- FIXME: on initial page load this is not setting `searchQuery` at all.
                    ie <- inputElement $ def
                      & inputElementConfig_initialValue .~ q
                      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("placeholder" =: "Search...")
                    let search :: Event t Text = tag (current $ value ie) (keypress Enter ie)
                    setRoute $ (Route_Search :/) <$> search
                    dyn_ $ ffor (value ie) $ \q' ->
                      routeLink' (Route_Search :/ q') "i" ("class" =: "search link icon") blank
                 
          divClass "ui segment" $ subRoute_ $ \case
            Route_Home -> el "p" $ do
              text "This app is a work in progress. Meanwhile, "
              routeLink (Route_Messages :/ fromGregorian 2019 3 27) $ text "start from 2019/3/27"
              text "?"
            Route_Search -> do
              query :: Dynamic t Text <- askRoute
              elClass "h1" "ui header" $ do
                text "Search results for "
                dynText query
              divClass "ui warning message" $ do
                divClass "header" $ text "Work in progress"
                text "Only a subset of results is being displayed"
              renderMessages =<< getMessages query ("/search-messages/" <>)
            Route_Messages -> do
              r :: Dynamic t Day <- askRoute
              elClass "h1" "ui header" $ do
                text "Archive for "
                dynText $ showDay <$> r
              divClass "" $ do
                dyn_ $ ffor r $ \day -> do
                  routeLink (Route_Messages :/ addDays (-1) day) $ elClass "button" "ui button" $ text "Prev Day"
                dyn_ $ ffor r $ \day -> do
                  routeLink (Route_Messages :/ addDays 1 day) $ elClass "button" "ui button" $ text "Next Day"
              renderMessages =<< getMessages r urlForBackendGetMessages
          divClass "ui bottom attached secondary segment" $ do
            elAttr "a" ("href" =: "https://github.com/srid/Taut") $ text "Powered by Haskell"
  }
  where
    getMessages
      :: (Reflex t, MonadHold t m, PostBuild t m, DomBuilder t m, Prerender js t m)
      => Dynamic t r -> (r -> Text) -> m (Event t (Maybe ([User], [Message])))
    getMessages r mkUrl = switchHold never <=< dyn $ ffor r $ \x -> do
      fmap switchDyn $ prerender (pure never) $ do
        pb <- getPostBuild
        getAndDecode $ mkUrl x <$ pb
    renderMessages msgsE =
      widgetHold_ (divClass "ui loading segment" blank) $ ffor msgsE $ divClass "ui segment" . \case
        Nothing -> text "Something went wrong"
        Just (users', msgs)
          | msgs == [] -> text "No messages for this day; try another day"
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
