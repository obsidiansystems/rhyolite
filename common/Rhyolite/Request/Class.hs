{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Rhyolite.Request.Class where

import Control.Monad ((>=>), guard, mzero)
import Data.Functor.Const
import Data.Functor.Sum
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Constraint (Dict (..), withDict)
import Data.Void (Void)

import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.GADT.Show
import Data.GADT.Show.TH

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

concat <$> mapM ($ ''ZeroRequest) [deriveGEq, deriveGCompare, deriveGShow]

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

instance Eq a => GEq (OneRequest a) where
  geq (OneRequest a0) (OneRequest a1) = Refl <$ guard (a0 == a1)

instance Ord a => GCompare (OneRequest a) where
  gcompare (OneRequest a0) (OneRequest a1) = case compare a0 a1 of
    LT -> GLT
    EQ -> GEQ
    GT -> GGT

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

instance (GEq f, GEq g) => GEq (GadtProduct f g) where
  geq (GadtPair f0 g0) (GadtPair f1 g1) = case (geq f0 f1, geq g0 g1) of
    (Just Refl, Just Refl) -> Just Refl
    _                      -> Nothing

instance (GCompare f, GCompare g) => GCompare (GadtProduct f g) where
  gcompare (GadtPair f0 g0) (GadtPair f1 g1) = case (gcompare f0 f1, gcompare g0 g1) of
    (GLT, _) -> GLT
    (GEQ, GLT) -> GLT
    (GEQ, GEQ) -> GEQ
    (GEQ, GGT) -> GGT
    (GGT, _) -> GGT

instance (GShow f, GShow g) => GShow (GadtProduct f g) where
  gshowsPrec p (GadtPair f g) = showString "GadtPair" . gshowsPrec p f . gshowsPrec p g

--------------------------------------------------------------------------------

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
