{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Common.Route where

import Prelude hiding (id, (.))

import Control.Category (Category (..))
import Control.Monad.Except
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Natural

import Obelisk.OAuth.Authorization
import Obelisk.Route
import Obelisk.Route.TH

import Common.Slack.Internal


newtype PaginatedRoute a = PaginatedRoute { unPaginatedRoute ::  (Natural, a) }
  deriving (Eq, Show, Ord)

mkPaginatedRouteAtPage1 :: a -> PaginatedRoute a
mkPaginatedRouteAtPage1 = PaginatedRoute . (1, )

paginatedRoutePageIndex :: PaginatedRoute a -> Natural
paginatedRoutePageIndex = fst . unPaginatedRoute

paginatedRouteValue :: PaginatedRoute a -> a
paginatedRouteValue = snd . unPaginatedRoute

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_OAuth :: BackendRoute (R OAuth)
  BackendRoute_GetSearchExamples :: BackendRoute ()
  BackendRoute_LocateMessage :: BackendRoute (Text, UTCTime) -- Channel and timestamp
  BackendRoute_SearchMessages :: BackendRoute (PaginatedRoute Text)

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_Search :: FrontendRoute (PaginatedRoute Text)

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_OAuth -> PathSegment "oauth" oauthRouteEncoder
      BackendRoute_GetSearchExamples -> PathSegment "get-search-examples" $ unitEncoder mempty
      BackendRoute_LocateMessage -> PathSegment "locate-message" locateMessageEncoder
      BackendRoute_SearchMessages -> PathSegment "search-messages" $
        paginatedEncoder textEncoderImpl
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FrontendRoute_Home -> PathEnd $ unitEncoder mempty
      FrontendRoute_Search -> PathSegment "search" $
        paginatedEncoder textEncoderImpl

-- TODO: compose instead of writing by hand
locateMessageEncoder
  :: (Applicative check, MonadError Text parse)
  => Encoder check parse (Text, UTCTime) PageName
locateMessageEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_decode = \([ch, t], _query) -> do
      (ch,) <$> parseSlackTimestamp t
  , _encoderImpl_encode = \(ch, t) -> ([ch, formatSlackTimestamp t], mempty)
  }

paginatedEncoder
  :: (Applicative check, MonadError Text parse)
  => EncoderImpl parse a [Text]
  -> Encoder check parse (PaginatedRoute a) PageName
paginatedEncoder aEncoderImpl = unsafeMkEncoder $ EncoderImpl
    { _encoderImpl_decode = \(p:ps, _query) -> do
        a <- _encoderImpl_decode aEncoderImpl [p]
        pure $ PaginatedRoute
          ( fromMaybe 1 $ read . T.unpack <$> listToMaybe ps
          , a
          )
    , _encoderImpl_encode = \(PaginatedRoute (n, a)) ->
        ( _encoderImpl_encode aEncoderImpl a <> (if n > 1 then [T.pack (show n)] else mempty)
        , mempty
        )
    }

-- TODO: shouldn't need this.
textEncoderImpl
  :: (MonadError Text parse)
  => EncoderImpl parse Text [Text]
textEncoderImpl = EncoderImpl
  { _encoderImpl_decode = \case
      [x] -> pure x
      _ -> throwError "textEncoderImpl: expected exactly 1 path element"
  , _encoderImpl_encode = \x -> [x]
  }

concat <$> mapM deriveRouteComponent
  [ ''FrontendRoute
  , ''BackendRoute
  ]
