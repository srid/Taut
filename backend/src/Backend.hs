 {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Control.Arrow ((&&&))
import Control.Exception.Safe (throwString)
import Data.Aeson
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Foldable (foldl')
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Control.Lens

import Database.Beam
import Database.Beam.Sqlite (runBeamSqlite)
import qualified Database.SQLite.Simple as SQLite

import Data.Pagination
import Snap

import Obelisk.Backend as Ob
import Obelisk.OAuth.Authorization
import Obelisk.Route hiding (decode, encode)

import Common.Route
import Common.Slack.Types
import Common.Slack.Internal
import Common.Slack.Types.Auth
import Common.Slack.Types.Search
import Common.Types

import Backend.Config
import Backend.Import (SlackDb (..), populateDatabase, slackDb)
import Backend.Login
import Backend.Search.Parser (parseSearchQuery)
import Backend.Search


backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_routeEncoder = backendRouteEncoder
  , _backend_run = \serve -> SQLite.withConnection "" $ \conn -> do
      team <- liftIO $ populateDatabase conn
      cfg <- readBackendConfig conn team
      liftIO $ T.putStrLn $ "routeEnv: " <> _backendConfig_routeEnv cfg

      serve $ \case
        BackendRoute_Missing :=> Identity () -> do
          writeLBS "404"
        BackendRoute_OAuth :=> Identity (OAuth_RedirectUri :=> Identity p) -> case p of
          Nothing -> liftIO $ throwString "Expected to receive the authorization code here"
          Just (RedirectUriParams code mstate) -> do
            handleOAuthCallback cfg code
            redirect $ T.encodeUtf8 $ fromMaybe (renderFrontendRoute (_backendConfig_enc cfg) $ FrontendRoute_Home :/ ()) mstate
        BackendRoute_GetSearchExamples :=> Identity () -> do
          resp :: ExamplesResponse <- authorizeUser cfg (renderFrontendRoute (_backendConfig_enc cfg) $ FrontendRoute_Home :/ ()) >>= \case
            Left e -> pure $ Left e
            Right u -> do
              -- TODO: This should be generic, and dates determined automatically.
              let examples :: [(Text, Text)] =
                    [ ("Do a basic search", "sunny day")
                    , ("Use quotes for exact match", "\"great day\"")
                    , ("Browse messages on a particular day", "during:2018-8-23")
                    ]
              pure $ Right (u, examples)
          writeLBS $ encode resp
        BackendRoute_LocateMessage :=> Identity t -> do
          authorizeUser cfg (renderBackendRoute (_backendConfig_enc cfg) $ BackendRoute_LocateMessage :/ t) >>= \case
            Left e -> redirect $ T.encodeUtf8 $ notAuthorizedLoginLink e
            Right _ -> do
              let day = utctDay t
                  mf = allMessages & messageFilters_during .~ [day]
              page <- liftIO $ runBeamSqlite (_backendConfig_sqliteConn cfg) $ do
                total <- countMessages cfg mf
                after <- countMessages cfg $ mf & messageFilters_at ?~ t
                let pgSize = fromIntegral $ _backendConfig_pageSize cfg
                pure $ fromIntegral $ 1 + (toInteger (total - after) `quot` pgSize)
              redirect $ T.encodeUtf8 $ (renderFrontendRoute (_backendConfig_enc cfg) $
                FrontendRoute_Search :/ PaginatedRoute (page, "during:" <> T.pack (show day))) <> "#" <> (formatSlackTimestamp t)
        BackendRoute_SearchMessages :=> Identity pQuery -> do
          resp :: MessagesResponse <- authorizeUser cfg (renderFrontendRoute (_backendConfig_enc cfg) $ FrontendRoute_Search :/ pQuery) >>= \case
            Left e -> pure $ Left e
            Right u -> do
              pagination <- liftIO $ mkPaginationFromRoute cfg pQuery
              case parseSearchQuery (paginatedRouteValue pQuery) of
                Left err -> do
                  liftIO $ putStrLn $ show err
                  -- TODO: return this error to user
                  liftIO $ throwString $ "Bad query: " <> show err
                Right q -> do
                  let mf = mkMessageFilters q
                  liftIO $ putStrLn $ show mf
                  msgs <- queryMessages cfg mf $ pagination
                  pure $ Right (u, (mf, msgs))
          writeLBS $ encode resp
  }
  where
    mkPaginationFromRoute cfg p = mkPagination (_backendConfig_pageSize cfg) (paginatedRoutePageIndex p)

    countMessages cfg mf = do
      liftIO $ runBeamSqlite (_backendConfig_sqliteConn cfg) $ do
        fmap (fromMaybe 0) $ runSelectReturningOne $
          select $ aggregate_ (\_ -> countAll_) $ messageFilters mf $ all_ (_slackMessages slackDb)
    queryMessages cfg mf (p :: Pagination) = do
      (users, msgs) <- liftIO $ runBeamSqlite (_backendConfig_sqliteConn cfg) $ do
        total <- fmap (fromMaybe 0) $ runSelectReturningOne $
          select $ aggregate_ (\_ -> countAll_) $ messageFilters mf $ all_ (_slackMessages slackDb)
        msgs :: Paginated Message <- paginate p (fromIntegral total) $ \offset limit -> do
          let q = limit_ limit $ offset_ offset $
                  orderBy_ (asc_ . _messageTs) $ messageFilters mf $ all_ (_slackMessages slackDb)
          -- liftIO $ dumpSqlSelect $ limit_ limit $ offset_ offset $
          --         orderBy_ (asc_ . _messageTs) $ messageFilters mf $ all_ (_slackMessages slackDb)
          runSelectReturningList $ select q
        users :: [User] <- runSelectReturningList $
          select $ all_ (_slackUsers slackDb)
        pure (users, msgs)
      pure $ flip fmap msgs $ \m -> m { _messageText = renderText users (_messageText m) }

    -- TODO: Extend this to render Slack format as well.
    renderText users s =
      foldl' (\m (userId, userName) -> T.replace userId userName m) s $ flip fmap users $ _userId &&& _userName
