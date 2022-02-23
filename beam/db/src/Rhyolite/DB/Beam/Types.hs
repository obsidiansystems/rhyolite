{-# language DeriveGeneric #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-|
   Description:
     Types and helpers for Beam that can be used in schemas and database interactions.
-}
module Rhyolite.DB.Beam.Types where

import Control.Lens
import Data.Proxy
import Database.Beam
import Rhyolite.DB.Beam.Orphans ()

-- | Like "Columnar", but a newtype instead of a type family, and the arguments
-- are in a more convenient order.
newtype WrapColumnar a f = WrapColumnar { unWrapColumnar :: Columnar f a }
  deriving (Generic)

instance Beamable (WrapColumnar a)

-- | Used to decorate a beam thing like PrimaryKey with Nullable
newtype WrapNullable k f = WrapNullable { unWrapNullable :: k (Nullable f) }
  deriving (Generic)

-- | Useful in 'Rhyolite.Task.Beam.Task' for "no data" in payload or result.
-- Every table has a subtable of type 'DummyTable'.
type DummyTable = (Proxy :: k -> *)

-- | Nicer name for 'Proxy' that explains what it's useful for in certain
-- contexts: A phantom standing in for a null part of a schema.
dummyTable :: forall k (x :: k). Proxy x
dummyTable = Proxy

-- | Every Beam table has a subtable of 'DummyTable'.
dummyTableLens :: forall k x. Lens' (k x) (DummyTable x)
dummyTableLens = lens
  (\_ -> Proxy)
  (\t _ -> t)
