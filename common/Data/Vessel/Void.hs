{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Vessel.Void where

import Data.Aeson
import GHC.Generics
import Data.Functor.Const
import Data.Functor.Identity
import Data.Patch (Group(..))
import Data.Vessel.Class
import Reflex.Query.Class

-- | completly empty View.
data VoidV (v :: * -> *) = VoidV
  deriving (Eq, Ord, Show, Generic)

instance FromJSON (VoidV a)
instance ToJSON (VoidV a)

instance Semigroup (VoidV x) where
  _ <> _ = VoidV
instance Monoid (VoidV x) where
  mempty = VoidV
instance Group (VoidV x) where
  negateG _ = VoidV

instance View VoidV where

instance Query (VoidV (Const x)) where
  type QueryResult (VoidV (Const x)) = VoidV Identity
  crop _ = id

instance EmptyView VoidV where
  emptyV = VoidV
