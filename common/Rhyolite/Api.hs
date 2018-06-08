{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rhyolite.Api where

import Data.Aeson (ToJSON, FromJSON, parseJSON, toJSON)
import Data.Constraint (Dict (..))

import Rhyolite.App (AppCredential, PublicRequest, PrivateRequest)
import Rhyolite.Request.Class (Request, SomeRequest (..), requestToJSON, requestParseJSON, requestResponseToJSON, requestResponseFromJSON)
import Rhyolite.HList (HList (HNil, HCons))

data ApiRequest :: * -> (k -> *) -> (k -> *) -> k -> * where
  ApiRequest_Public :: public a -> ApiRequest cred public private a
  ApiRequest_Private :: cred -> private a -> ApiRequest cred public private a
  deriving (Show)

type AppRequest app = ApiRequest (AppCredential app) (PublicRequest app) (PrivateRequest app)

instance (Request private, Request public, ToJSON cred, FromJSON cred) => Request (ApiRequest cred public private) where
  requestToJSON r = case r of
    ApiRequest_Public p -> case (requestResponseToJSON p, requestResponseFromJSON p) of
      (Dict, Dict) -> toJSON ("Public"::String, SomeRequest p `HCons` HNil)
    ApiRequest_Private token p -> case (requestResponseToJSON p, requestResponseFromJSON p) of
      (Dict, Dict) -> toJSON ("Private"::String, token `HCons` SomeRequest p `HCons` HNil)
  requestParseJSON v = do
    (tag, body) <- parseJSON v
    case tag of
      ("Public"::String) -> do
        SomeRequest p `HCons` HNil <- parseJSON body
        return $ SomeRequest $ ApiRequest_Public p
      ("Private"::String) -> do
        token `HCons` SomeRequest p `HCons` HNil <- parseJSON body
        return $ SomeRequest $ ApiRequest_Private token p
      e -> error $ "Could not parse tag: " ++ e
  requestResponseToJSON = \case
    ApiRequest_Public p -> requestResponseToJSON p
    ApiRequest_Private _ p -> requestResponseToJSON p
  requestResponseFromJSON = \case
    ApiRequest_Public p -> requestResponseFromJSON p
    ApiRequest_Private _ p -> requestResponseFromJSON p

public :: PublicRequest app t -> AppRequest app t
public = ApiRequest_Public

private :: AppCredential app -> PrivateRequest app t -> AppRequest app t
private = ApiRequest_Private
