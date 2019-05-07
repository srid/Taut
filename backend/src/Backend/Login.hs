{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- Slack login and how we use that to authorize users
module Backend.Login
  ( authorizeUser
  , handleOAuthCallback
  ) where

import Control.Exception.Safe (throwString)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (catMaybes, listToMaybe)
import Data.Profunctor (rmap)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import Network.HTTP.Client
import Snap
import Snap.Snaplet.Session
import Web.ClientSession

import Obelisk.OAuth.AccessToken
import Obelisk.OAuth.Authorization
import Obelisk.Route hiding (decode, encode)

import Common.Route
import Common.Slack.Types.Auth

import Backend.Config


authorizeUser
  :: MonadSnap m
  => BackendConfig
  -> R Route  -- ^ The route to redirect after signing in to Slack
  -> m (Either NotAuthorized SlackUser)
authorizeUser cfg r = do
  tok <- getAuthToken (_backendConfig_sessKey cfg)
  pure $ rmap allowAnonymousOnLocalhost f tok
  where
    f = \case
      Nothing ->
        Left $ NotAuthorized_RequireLogin ll
      Just (_, v) -> case decode v of
        Nothing -> Left $ NotAuthorized_RequireLogin ll
        Just t -> if
          | not (_slackTokenResponse_ok t) -> Left $ NotAuthorized_RequireLogin ll
          | _slackTokenResponse_team t /= _backendConfig_team cfg ->
            Left $ NotAuthorized_WrongTeam (_backendConfig_team cfg) ll
          | otherwise -> Right $ _slackTokenResponse_user t


    -- NOTE: The only reason we build the grantHref in the backend instead
    -- of the frontend (where it would be most appropriate) is because of a
    -- bug in obelisk missing exe-config (we need routeEnv) in the frontend
    -- post hydration.
    ll = mkSlackLoginLink cfg $ Just $ renderFrontendRoute (_backendConfig_enc cfg) r

    allowAnonymousOnLocalhost
      :: Either NotAuthorized SlackUser
      -> Either NotAuthorized SlackUser
    allowAnonymousOnLocalhost = if T.isPrefixOf "http://localhost:" (_backendConfig_routeEnv cfg)
      then Right . either (const dummyUser) id
      else id
      where
        dummyUser = SlackUser "localbody" "U11111111"

setSlackTokenToCookie :: MonadSnap m => BackendConfig -> SlackTokenResponse -> m ()
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

handleOAuthCallback :: MonadSnap m => BackendConfig -> Text -> m ()
handleOAuthCallback cfg code = do
  let t = TokenRequest
        { _tokenRequest_grant = TokenGrant_AuthorizationCode $ T.encodeUtf8 code
        , _tokenRequest_clientId = _backendConfig_oauthClientID cfg
        , _tokenRequest_clientSecret = _backendConfig_oauthClientSecret cfg
        , _tokenRequest_redirectUri = BackendRoute_OAuth
        }
      reqUrl = "https://slack.com/api/oauth.access"
  req <- liftIO $ getOauthToken
    reqUrl
    (_backendConfig_routeEnv cfg)
    (_backendConfig_enc cfg)
    t
  resp <- liftIO $ httpLbs req $ _backendConfig_tlsMgr cfg
  -- TODO: check response errors (both code and body json)!
  case decode (responseBody resp) of
    Nothing -> do
      -- TODO: When this throws {ok: false, error: ...} parse that properly
      liftIO $ T.putStrLn $ T.decodeUtf8 $ BL.toStrict $ responseBody resp
      liftIO $ throwString "Unable to decode JSON from Slack oauth.access response"
    Just str -> do
      setSlackTokenToCookie cfg str

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
