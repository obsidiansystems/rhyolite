{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Rhyolite.Request.Class where

import Control.Monad ((>=>), mzero)
import Data.Functor.Const
import Data.Functor.Sum
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Constraint (Dict (..), withDict)
import Data.Void (Void)

data SomeRequest t where
    SomeRequest :: (FromJSON x, ToJSON x) => t x -> SomeRequest t

class Request r where
  requestToJSON :: r a -> Value
  requestParseJSON :: Value -> Parser (SomeRequest r)
  requestResponseToJSON :: r a -> Dict (ToJSON a)
  requestResponseFromJSON :: r a -> Dict (FromJSON a)

instance Request r => FromJSON (SomeRequest r) where
  parseJSON = requestParseJSON

instance Request r => ToJSON (SomeRequest r) where
  toJSON (SomeRequest r) = requestToJSON r

--------------------------------------------------------------------------------

data ZeroRequest :: * -> * where
  ZeroRequest :: Void -> ZeroRequest Void

instance Request ZeroRequest where
  requestToJSON (ZeroRequest e) = case e of {}
  requestParseJSON _ = mzero
  requestResponseToJSON (ZeroRequest e) = case e of {}
  requestResponseFromJSON (ZeroRequest e) = case e of {}

--------------------------------------------------------------------------------

-- | Instantiate 'a' with '()' for true 1.
--
-- TODO make newtype when GHC is smarter.
data OneRequest a :: * -> * where
  OneRequest :: a -> OneRequest a ()

instance (ToJSON a, FromJSON a) => Request (OneRequest a) where
  requestToJSON (OneRequest a) = toJSON a
  requestParseJSON = fmap (SomeRequest . OneRequest) . parseJSON
  requestResponseToJSON (OneRequest _) = Dict
  requestResponseFromJSON (OneRequest _) = Dict

--------------------------------------------------------------------------------

-- | Combine two Request specifications into a new request specification
-- containing the routes of each.
instance (Request f, Request g) => Request (Sum f g) where
  requestToJSON v = withDict d $ toJSON v'
    where
      v' :: Sum (Const Value) (Const Value) ()
      (d, v') = case v of
        InL x -> (requestResponseToJSON x, InL $ Const $ requestToJSON x)
        InR x -> (requestResponseToJSON x, InR $ Const $ requestToJSON x)

  requestParseJSON = liftParseJSON (const mzero) (const mzero) >=> \case
    InL (Const value :: Const Value ()) -> do
      SomeRequest req <- requestParseJSON value
      pure $ SomeRequest $ InL req
    InR (Const value) -> do
      SomeRequest req <- requestParseJSON value
      pure $ SomeRequest $ InR req

  requestResponseToJSON = \case
    InL x -> requestResponseToJSON x
    InR x -> requestResponseToJSON x
  requestResponseFromJSON = \case
    InL x -> requestResponseFromJSON x
    InR x -> requestResponseFromJSON x

--------------------------------------------------------------------------------

data GadtProduct f g :: * -> * where
  GadtPair :: forall a b f g. f a -> g b -> GadtProduct f g (a, b)

-- | Combine two Request specifications pairwise into a new request
-- specification where each request is a pair of old requests (one from each),
-- and each response is the pair of the two responses.
instance (Request f, Request g) => Request (GadtProduct f g) where
  requestToJSON :: forall a. GadtProduct f g a -> Value
  requestToJSON (GadtPair x y) = toJSON (requestToJSON x, requestToJSON y)

  requestParseJSON v = do
    (v0 :: Value, v1 :: Value) <- parseJSON v
    SomeRequest f <- requestParseJSON v0
    SomeRequest g <-requestParseJSON v1
    pure $ SomeRequest $ GadtPair f g

  requestResponseToJSON (GadtPair (requestResponseToJSON -> Dict)
                                  (requestResponseToJSON -> Dict)) = Dict
  requestResponseFromJSON (GadtPair (requestResponseFromJSON -> Dict)
                                    (requestResponseFromJSON -> Dict)) = Dict
