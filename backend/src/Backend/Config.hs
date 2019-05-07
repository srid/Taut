{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Config where

import Control.Exception.Safe (Exception, throwIO)
import Data.Text (Text)
import Data.Functor.Sum
import Data.Functor.Identity (Identity)
import GHC.Natural (Natural)
import qualified Data.Text as T

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Web.ClientSession
import Obelisk.Route hiding (decode, encode)
import qualified Obelisk.ExecutableConfig as Cfg

import Common.Route
import Common.Slack.Types.Auth (SlackTeam(..))

data BackendConfig = BackendConfig
  { _backendConfig_enc :: Encoder Identity Identity (R (Sum BackendRoute (ObeliskRoute Route))) PageName
  , _backendConfig_sessKey :: Key
  , _backendConfig_tlsMgr :: Manager
  , _backendConfig_team :: SlackTeam
  , _backendConfig_pageSize :: Natural
  , _backendConfig_routeEnv :: Text
  , _backendConfig_oauthClientID :: Text  -- TODO: invariant for oauth format?
  , _backendConfig_oauthClientSecret :: Text
  }

data InvalidConfig
  = InvalidConfig_Missing Text
  | InvalidConfig_Empty Text
  deriving (Eq, Show, Ord)

instance Exception InvalidConfig

readBackendConfig :: SlackTeam -> IO BackendConfig
readBackendConfig team = BackendConfig enc
  <$> (snd <$> randomKey)
  <*> newTlsManager
  <*> pure team
  <*> pure defaultPageSize
  <*> getConfigNonEmpty "config/common/route"
  <*> getConfigNonEmpty "config/backend/oauthClientID"
  <*> getConfigNonEmpty "config/backend/oauthClientSecret"
  where
    Right (enc :: Encoder Identity Identity (R (Sum BackendRoute (ObeliskRoute Route))) PageName) = checkEncoder backendRouteEncoder
    defaultPageSize = 30
    getConfigNonEmpty p = Cfg.get p >>= \case
      Nothing -> throwIO $ InvalidConfig_Missing p
      Just v' -> do
        let v = T.strip v'
        if T.null v then throwIO (InvalidConfig_Empty p) else pure v
