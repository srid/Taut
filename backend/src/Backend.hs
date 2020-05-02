{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Backend where

import Backend.Config
import Backend.Import (SlackDb (..), populateDatabase, slackDb)
import Backend.Login
import Backend.Search
import Backend.Search.Parser (parseSearchQuery)
import Common.Route
import Common.Slack.Types
import Common.Slack.Types.Search
import Common.Types
import Control.Arrow ((&&&))
import Control.Exception.Safe (throwString)
import Control.Lens
import Data.Aeson
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Foldable (foldl')
import Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Pagination
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Database.Beam
import Database.Beam.Sqlite (runBeamSqlite)
import qualified Database.SQLite.Simple as SQLite
import Obelisk.Backend (Backend (..))
import Obelisk.ExecutableConfig.Lookup
import Obelisk.OAuth.Authorization
import Obelisk.Route hiding (decode, encode)
import Snap

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_routeEncoder = fullRouteEncoder,
      _backend_run = \serve -> do
        liftIO $ SQLite.withConnection "" $ \conn -> do
          configs <- getConfigs
          let Just slackExportPath =
                T.strip . T.decodeUtf8
                  <$> Map.lookup "backend/slackExportPath" configs
          team <- liftIO $ populateDatabase conn slackExportPath
          cfg <- readBackendConfig conn team
          liftIO $ T.putStrLn $ "routeEnv: " <> _backendConfig_routeEnv cfg
          serve $ requestHandler cfg
    }
  where
    requestHandler :: MonadSnap m => BackendConfig -> R BackendRoute -> m ()
    requestHandler cfg = \case
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
                  [ ("Do a basic search", "sunny day"),
                    ("Use quotes for exact match", "\"great day\""),
                    ("Browse messages on a particular day", "during:2018-8-23"),
                    ("All messages in #general channel", "in:general"),
                    ("Messages by Andrew in #random channel", "in:random from:andrew"),
                    ("Messages in #general after August 2018", "after:2018-08-01 in:general"),
                    ("Messages in #random during mid 2016", "after:2016-08-01 before:2016-09-01 in:random")
                  ]
            pure $ Right (u, examples)
        writeLBS $ encode resp
      BackendRoute_SearchMessages :=> Identity pQuery -> do
        resp :: MessagesResponse <- authorizeUser cfg (renderFrontendRoute (_backendConfig_enc cfg) $ FrontendRoute_Search :/ pQuery) >>= \case
          Left e -> pure $ Left e
          Right u -> do
            case parseSearchQuery (paginatedRouteValue pQuery) of
              Left err -> do
                liftIO $ putStrLn $ show err
                pure $ Right (u, Left ())
              Right q -> do
                let mf = mkMessageFilters q
                liftIO $ putStrLn $ show mf
                pagination <- liftIO $ mkPaginationFromRoute cfg mf pQuery
                msgs <- queryMessages cfg mf $ pagination
                pure $ Right $ (u, Right (mf, msgs))
        writeLBS $ encode resp
    mkPaginationFromRoute cfg mf p = mkPagination (_backendConfig_pageSize cfg) =<< case paginatedRouteCursor p of
      Left pg -> pure pg
      Right t -> locateMessage cfg mf t
    locateMessage cfg mf t = do
      liftIO $ runBeamSqlite (_backendConfig_sqliteConn cfg) $ do
        total <- countMessages cfg mf
        after <- countMessages cfg $ mf & messageFilters_at ?~ t
        let pgSize = fromIntegral $ _backendConfig_pageSize cfg
        pure $ fromIntegral $ 1 + (toInteger (total - after) `quot` pgSize)
    countMessages cfg mf = do
      liftIO $ runBeamSqlite (_backendConfig_sqliteConn cfg) $ do
        fmap (fromMaybe 0) $ runSelectReturningOne
          $ select
          $ aggregate_ (\_ -> countAll_)
          $ messageFilters mf
          $ all_ (_slackMessages slackDb)
    queryMessages cfg mf (p :: Pagination) = do
      (users, msgs) <- liftIO $ runBeamSqlite (_backendConfig_sqliteConn cfg) $ do
        total <-
          fmap (fromMaybe 0) $ runSelectReturningOne
            $ select
            $ aggregate_ (\_ -> countAll_)
            $ messageFilters mf
            $ all_ (_slackMessages slackDb)
        msgs :: Paginated Message <- paginate p (fromIntegral total) $ \offset limit -> do
          let q =
                limit_ limit $ offset_ offset
                  $ orderBy_ (asc_ . _messageTs)
                  $ messageFilters mf
                  $ all_ (_slackMessages slackDb)
          -- liftIO $ dumpSqlSelect $ limit_ limit $ offset_ offset $
          --         orderBy_ (asc_ . _messageTs) $ messageFilters mf $ all_ (_slackMessages slackDb)
          runSelectReturningList $ select q
        users :: [User] <-
          runSelectReturningList
            $ select
            $ all_ (_slackUsers slackDb)
        pure (users, msgs)
      pure $ flip fmap msgs $ \m -> m {_messageText = renderText users (_messageText m)}
    -- TODO: Extend this to render Slack format as well.
    renderText users s =
      foldl' (\m (userId, userName) -> T.replace userId userName m) s $ flip fmap users $ _userId &&& _userName
