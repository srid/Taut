{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Config where

import Control.Monad.IO.Class
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Text.Encoding (decodeUtf8)
import GHC.Natural (Natural)
import GHC.Stack

import qualified Database.SQLite.Simple as SQLite
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Web.ClientSession

import Obelisk.ExecutableConfig.Lookup
import Obelisk.Route hiding (decode, encode)

import Common.Route
import Common.Slack.Types.Auth (SlackTeam (..))

data BackendConfig = BackendConfig
  { _backendConfig_enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
  , _backendConfig_sessKey :: !Key
  , _backendConfig_tlsMgr :: !Manager
  , _backendConfig_sqliteConn :: !SQLite.Connection
  , _backendConfig_team :: !SlackTeam
  , _backendConfig_pageSize :: !Natural
  , _backendConfig_routeEnv :: !Text
  , _backendConfig_oauthClientID :: !Text  -- TODO: invariant for oauth format?
  , _backendConfig_oauthClientSecret :: !Text
  , _backendConfig_slackExportPath :: !Text
  }

readBackendConfig
  :: (HasCallStack, MonadIO m)
  => SQLite.Connection -> SlackTeam -> m BackendConfig
readBackendConfig conn team = do
  configs <- liftIO $ getConfigs
  let Just route = decodeUtf8 <$> Map.lookup "common/route" configs
  let Just oauthClientID = T.strip . decodeUtf8 <$> Map.lookup "backend/oauthClientID" configs
  let Just oauthClientSecret = T.strip . decodeUtf8 <$> Map.lookup "backend/oauthClientSecret" configs
  let Just slackExportPath = T.strip . decodeUtf8 <$> Map.lookup "backend/slackExportPath" configs
  k <- snd <$> liftIO randomKey
  tlsMgr <- liftIO newTlsManager
  pure $ BackendConfig enc k tlsMgr conn team defaultPageSize route oauthClientID oauthClientSecret slackExportPath
  where
    Right (enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName) = checkEncoder fullRouteEncoder
    -- WARNING: Changing the default page size will invalidate existing message
    -- permalinks (which contain page index embedded in them).
    --
    -- The only solution here is to get rid of page-index pagination and use
    -- ?afterTs or some such thing, while making sure frontend page navigation
    -- is still possible.
    defaultPageSize = 30
