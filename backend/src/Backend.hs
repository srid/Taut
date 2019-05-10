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
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

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
import Common.Types

import Backend.Config
import Backend.Import (SlackDb (..), populateDatabase, slackDb)
import Backend.Login
import Backend.Query


backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_routeEncoder = backendRouteEncoder
  , _backend_run = \serve -> SQLite.withConnection "" $ \conn -> do
      team <- liftIO $ populateDatabase conn
      cfg <- readBackendConfig conn team
      liftIO $ T.putStrLn $ "routeEnv: " <> _backendConfig_routeEnv cfg

      serve $ \case
        BackendRoute_Missing :/ () -> do
          writeLBS "404"
        BackendRoute_OAuth :/ OAuth_RedirectUri :/ p -> case p of
          Nothing -> liftIO $ throwString "Expected to receive the authorization code here"
          Just (RedirectUriParams code mstate) -> do
            handleOAuthCallback cfg code
            redirect $ T.encodeUtf8 $ fromMaybe (renderFrontendRoute (_backendConfig_enc cfg) $ FrontendRoute_Home :/ ()) mstate
        BackendRoute_GetMessages :/ pDay -> do
          -- TODO: Use MonadError wherever possible
          resp :: MessagesResponse <- authorizeUser cfg (FrontendRoute_Messages :/ pDay) >>= \case
            Left e -> pure $ Left e
            Right t -> do
              pagination <- liftIO $ mkPaginationFromRoute cfg pDay
              resp <- queryMessages cfg allMessages $ pagination -- TODO
              pure $ Right (t, resp)
          writeLBS $ encode resp
        BackendRoute_SearchMessages :/ pQuery -> do
          resp :: MessagesResponse <- authorizeUser cfg (FrontendRoute_Search :/ pQuery) >>= \case
            Left e -> pure $ Left e
            Right t -> do
              pagination <- liftIO $ mkPaginationFromRoute cfg pQuery
              let mf = mkMessageFilters $ either (throwString . show) id $ parseSearchQuery $ paginatedRouteValue pQuery
              resp <- queryMessages cfg mf $ pagination
              pure $ Right (t, resp)
          writeLBS $ encode resp
        _ -> undefined -- FIXME: wtf why does the compiler require this?
  }
  where
    mkPaginationFromRoute cfg p = mkPagination (_backendConfig_pageSize cfg) (paginatedRoutePageIndex p)

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
