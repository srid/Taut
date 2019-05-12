{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Config where

import Control.Exception.Safe (Exception, throwIO)
import Data.Functor.Identity (Identity)
import Data.Functor.Sum
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Natural (Natural)

import qualified Database.SQLite.Simple as SQLite
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Web.ClientSession

import qualified Obelisk.ExecutableConfig as Cfg
import Obelisk.Route hiding (decode, encode)

import Common.Route
import Common.Slack.Types.Auth (SlackTeam (..))

data BackendConfig = BackendConfig
  { _backendConfig_enc :: Encoder Identity Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
  , _backendConfig_sessKey :: Key
  , _backendConfig_tlsMgr :: Manager
  , _backendConfig_sqliteConn :: SQLite.Connection
  , _backendConfig_team :: SlackTeam
  , _backendConfig_pageSize :: Natural
  , _backendConfig_routeEnv :: Text
  , _backendConfig_oauthClientID :: Text  -- TODO: invariant for oauth format?
  , _backendConfig_oauthClientSecret :: Text
  , _backendConfig_slackExportPath :: Text
  }

data InvalidConfig
  = InvalidConfig_Missing Text
  | InvalidConfig_Empty Text
  deriving (Eq, Show, Ord)

instance Exception InvalidConfig

readBackendConfig :: SQLite.Connection -> SlackTeam -> IO BackendConfig
readBackendConfig conn team = BackendConfig enc
  <$> (snd <$> randomKey)
  <*> newTlsManager
  <*> pure conn
  <*> pure team
  <*> pure defaultPageSize
  <*> getConfigNonEmpty "config/common/route"
  <*> getConfigNonEmpty "config/backend/oauthClientID"
  <*> getConfigNonEmpty "config/backend/oauthClientSecret"
  <*> getSlackExportPath
  where
    Right (enc :: Encoder Identity Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName) = checkEncoder backendRouteEncoder
    -- WARNING: Changing the default page size will invalidate existing message
    -- permalinks (which contain page index embedded in them).
    --
    -- The only solution here is to get rid of page-index pagination and use
    -- ?afterTs or some such thing, while making sure frontend page navigation
    -- is still possible.
    defaultPageSize = 30

getSlackExportPath :: IO Text
getSlackExportPath = getConfigNonEmpty "config/backend/slackExportPath"

getConfigNonEmpty :: Text -> IO Text
getConfigNonEmpty p = Cfg.get p >>= \case
  Nothing -> throwIO $ InvalidConfig_Missing p
  Just v' -> do
    let v = T.strip v'
    if T.null v then throwIO (InvalidConfig_Empty p) else pure v
