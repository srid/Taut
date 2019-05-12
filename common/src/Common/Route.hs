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
import qualified Data.Map as Map
import GHC.Natural

import Obelisk.OAuth.Authorization
import Obelisk.Route
import Obelisk.Route.TH

import Common.Slack.Internal


newtype PaginatedRoute cursor a = PaginatedRoute
  { unPaginatedRoute ::  (Either Natural cursor, a)
  }
  deriving (Eq, Show, Ord)

mkPaginatedRouteAtPage1 :: a -> PaginatedRoute cursor a
mkPaginatedRouteAtPage1 = PaginatedRoute . (Left 1, )

paginatedRouteCursor :: PaginatedRoute cursor a -> Either Natural cursor
paginatedRouteCursor = fst . unPaginatedRoute

paginatedRouteValue :: PaginatedRoute cursor a -> a
paginatedRouteValue = snd . unPaginatedRoute

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_OAuth :: BackendRoute (R OAuth)
  BackendRoute_GetSearchExamples :: BackendRoute ()
  BackendRoute_SearchMessages :: BackendRoute (PaginatedRoute UTCTime Text)

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_Search :: FrontendRoute (PaginatedRoute UTCTime Text)

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_OAuth -> PathSegment "oauth" oauthRouteEncoder
      BackendRoute_GetSearchExamples -> PathSegment "get-search-examples" $ unitEncoder mempty
      BackendRoute_SearchMessages -> PathSegment "search-messages" $
        paginatedEncoder utcTimeEncoderImpl textEncoderImpl
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FrontendRoute_Home -> PathEnd $ unitEncoder mempty
      FrontendRoute_Search -> PathSegment "search" $
        paginatedEncoder utcTimeEncoderImpl textEncoderImpl

-- TODO: compose instead of writing by hand
paginatedEncoder
  :: (Applicative check, MonadError Text parse)
  => EncoderImpl parse (Maybe cursor) [Text]
  -> EncoderImpl parse a [Text]
  -> Encoder check parse (PaginatedRoute cursor a) PageName
paginatedEncoder cursorEncoderImpl aEncoderImpl = unsafeMkEncoder $ EncoderImpl
    { _encoderImpl_decode = \(p:ps, query) -> do
        mcursor <- _encoderImpl_decode cursorEncoderImpl ps
        a <- _encoderImpl_decode aEncoderImpl [p]
        case mcursor of
          Nothing -> pure $ PaginatedRoute
            ( Left $ fromMaybe 1 $ fmap (read . T.unpack) =<< Map.lookup "p" query
            , a
            )
          Just cursor -> pure $ PaginatedRoute
            ( Right cursor
            , a
            )
    , _encoderImpl_encode = \(PaginatedRoute (nc, a)) ->
        ( _encoderImpl_encode aEncoderImpl a <> (either mempty (_encoderImpl_encode cursorEncoderImpl . Just) nc)
        , either (Map.singleton "p" . Just . T.pack . show) mempty nc
        )
    }

-- TODO: shouldn't need the Impl implementations with
-- https://github.com/obsidiansystems/obelisk/pull/426/files
utcTimeEncoderImpl
  :: (MonadError Text parse)
  => EncoderImpl parse (Maybe UTCTime) [Text]
utcTimeEncoderImpl = EncoderImpl
  { _encoderImpl_decode = \ps -> case ps of
      [] -> pure Nothing
      [t] -> Just <$> parseSlackTimestamp t
      _ -> throwError "More than 1 path element for utcTimeEncoderImpl"
  , _encoderImpl_encode = \mt -> (maybe [] (pure . formatSlackTimestamp) mt)
  }

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
