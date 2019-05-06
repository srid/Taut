{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- Slack login and how we use that to authorize users
module Backend.Login where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Data.Profunctor (rmap)
import qualified Data.Text as T

import Snap
import Snap.Snaplet.Session
import Web.ClientSession

import Obelisk.OAuth.Authorization
import Obelisk.Route hiding (decode, encode)

import Common.Route
import Common.Slack.Types.Auth

import Backend.Config

allowAnonymousOnLocalhost
  :: BackendConfig
  -> Either Text (Maybe SlackTokenResponse)
  -> Either Text (Maybe SlackTokenResponse)
allowAnonymousOnLocalhost cfg = if T.isPrefixOf "http://localhost:" (_backendConfig_routeEnv cfg)
  then Right . either (const $ Just aWithLocal) id
  else id
  where
    aWithLocal = SlackTokenResponse
      { _slackTokenResponse_ok = True
      , _slackTokenResponse_accessToken = "xoxp-dummy"
      , _slackTokenResponse_scope = "identity.basic"
      , _slackTokenResponse_user = SlackUser "localbody" "U11111111"
      , _slackTokenResponse_team = SlackTeam "T11111111"
      }

getSlackTokenFromCookie
  :: MonadSnap m
  => BackendConfig
  -> R Route  -- ^ The route to redirect after signing in to Slack
  -> m (Either Text (Maybe SlackTokenResponse))
getSlackTokenFromCookie cfg r =
  rmap (allowAnonymousOnLocalhost cfg) f <$> getAuthToken (_backendConfig_sessKey cfg)
  where
    f = \case
      Nothing ->
        -- NOTE: The oreason we build the grantHref in the backend instead
        -- of the frontend (where it would be most appropriate) is because of a
        -- bug in obelisk missing exe-config (we need routeEnv) in the frontend
        -- post hydration.
        Left $ mkSlackLoginLink cfg $ Just $ renderFrontendRoute (_backendConfig_enc cfg) r
      Just (_, v) ->
        Right $ decode v
  
-- FIXME: Why is this a Maybe?
setSlackTokenToCookie :: MonadSnap m => BackendConfig -> Maybe SlackTokenResponse -> m ()
setSlackTokenToCookie cfg = setAuthToken (_backendConfig_sessKey cfg) . encode

mkSlackLoginLink :: BackendConfig -> Maybe Text -> Text
mkSlackLoginLink cfg mstate = authorizationRequestHref authUrl routeEnv enc r
  where
    r = AuthorizationRequest
        { _authorizationRequest_responseType = AuthorizationResponseType_Code
        , _authorizationRequest_clientId = _backendConfig_oauthClientID cfg
        , _authorizationRequest_redirectUri = Nothing -- Just BackendRoute_OAuth
        , _authorizationRequest_scope = ["identity.basic"]
        , _authorizationRequest_state = mstate
        }
    authUrl = "https://slack.com/oauth/authorize"
    routeEnv = _backendConfig_routeEnv cfg
    enc = _backendConfig_enc cfg

getAuthToken :: MonadSnap m => Key -> m (Maybe (SecureCookie BL.ByteString))
getAuthToken k = do
  c <- getsRequest rqCookies
  fmap (join . listToMaybe . catMaybes) $ forM c $ \cc->
    if cookieName cc == "tautAuthToken"
        then let x :: Maybe (SecureCookie BL.ByteString) = decodeSecureCookie @BL.ByteString k (cookieValue cc)
             in pure $ Just x
        else pure Nothing

setAuthToken :: MonadSnap m => Key -> BL.ByteString -> m ()
setAuthToken k t = setSecureCookie "tautAuthToken" Nothing k Nothing (t :: BL.ByteString)
