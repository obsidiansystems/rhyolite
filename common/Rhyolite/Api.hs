{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Rhyolite.Api where

import Data.Aeson (ToJSON, FromJSON, parseJSON, toJSON)
import Data.Constraint.Extras
import Data.Some as Some

-- TODO: Make a Vessel key for this kind of thing and use VSum for such requests.
data ApiRequest :: * -> (k -> *) -> (k -> *) -> k -> * where
  ApiRequest_Public :: public a -> ApiRequest cred public private a
  ApiRequest_Private :: cred -> private a -> ApiRequest cred public private a
  deriving (Show)

public :: public a -> ApiRequest cred public private a
public = ApiRequest_Public

private :: cred -> private a -> ApiRequest cred public private a
private = ApiRequest_Private

instance (ArgDict public, ArgDict private) => ArgDict (ApiRequest t public private) where
  type ConstraintsFor (ApiRequest t public private) c = (ConstraintsFor public c, ConstraintsFor private c)
  type ConstraintsFor' (ApiRequest t public private) c g = (ConstraintsFor' public c g, ConstraintsFor' private c g)
  argDict (ApiRequest_Public x) = argDict x
  argDict (ApiRequest_Private _ x) = argDict x
  argDict' (ApiRequest_Public x) = argDict' x
  argDict' (ApiRequest_Private _ x) = argDict' x

instance (ToJSON cred, ToJSON (public a), ToJSON (private a)) => ToJSON (ApiRequest cred public private a) where
  toJSON (ApiRequest_Public x) = toJSON (toJSON "Public", toJSON x)
  toJSON (ApiRequest_Private t x) = toJSON (toJSON "Private", toJSON (t, x))

instance (FromJSON cred, FromJSON (Some public), FromJSON (Some private)) => FromJSON (Some (ApiRequest cred public private)) where
  parseJSON v = do
    (tag, rest) <- parseJSON v
    case tag of
      "Public" -> do
        Some.This x <- parseJSON rest
        return (Some.This (ApiRequest_Public x))
      "Private" -> do
        (t, Some.This x) <- parseJSON rest
        return (Some.This (ApiRequest_Private t x))
      _ -> fail $ "parseJSON for ApiRequest: unrecognised tag: " ++ tag
