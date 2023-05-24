{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Beam.Patch.Decoder
  ( GPatchDatabase
  , defaultDatabasePatchDecoder
  , EntityPatchDecoder (..)
  , FieldDecoder (..)
  , module Obelisk.Beam.Patch.Types
  ) where

import qualified GHC.Generics as Generic
import GHC.Generics hiding (R, C)

import Database.Beam.Backend.Types
import Database.Beam.Schema.Tables
import Data.Functor.Identity
import Obelisk.Beam.Patch.Types

data EntityPatchDecoder be e where
  EntityPatchDecoder_NotDecodable :: EntityPatchDecoder be e
  EntityPatchDecoder_Table :: (Table tbl, Ord (PrimaryKey tbl Identity), Eq (tbl Maybe)) => tbl (FieldDecoder be) -> EntityPatchDecoder be (TableEntity tbl)

data FieldDecoder be a where
  FieldDecoder :: BackendFromField be a => FieldDecoder be a

class GPatchDatabase x where
  gDatabasePatchDecoder :: x

instance GPatchDatabase (x p) => GPatchDatabase (D1 f x p) where
  gDatabasePatchDecoder = M1 gDatabasePatchDecoder

instance GPatchDatabase (x p) => GPatchDatabase (C1 f x p) where
  gDatabasePatchDecoder = M1 gDatabasePatchDecoder

instance (GPatchDatabase (x p), GPatchDatabase (y p)) => GPatchDatabase ((x :*: y) p) where
  gDatabasePatchDecoder = gDatabasePatchDecoder :*: gDatabasePatchDecoder

instance PatchEntity be a => GPatchDatabase (S1 f (K1 Generic.R (EntityPatchDecoder be a)) p) where
  gDatabasePatchDecoder = M1 $ K1 patchEntity

instance ( Generic (db (EntityPatchDecoder be))
         , GPatchDatabase (Rep (db (EntityPatchDecoder be)) ())
         ) => GPatchDatabase (S1 f (K1 Generic.R (db (EntityPatchDecoder be))) p) where
  gDatabasePatchDecoder = M1 $ K1 $ to' gDatabasePatchDecoder

type family FieldPatches be x where
  FieldPatches be (D1 f x) = D1 f (FieldPatches be x)
  FieldPatches be (C1 f x) = C1 f (FieldPatches be x)
  FieldPatches be (x :*: y) = FieldPatches be x :*: FieldPatches be y
  FieldPatches be (S1 f (K1 Generic.R (Exposed a))) = S1 f (K1 Generic.R (FieldDecoder be a))
  FieldPatches be (S1 f (K1 Generic.R (tbl Exposed))) = S1 f (K1 Generic.R (tbl (FieldDecoder be)))
  FieldPatches be (S1 f (K1 Generic.R (tbl (Nullable Exposed)))) = S1 f (K1 Generic.R (tbl (Nullable (FieldDecoder be))))

class GPatchFields be exposedRep where
  gPatchFields :: FieldPatches be exposedRep ()

instance GPatchFields be x => GPatchFields be (D1 f x) where
  gPatchFields = M1 $ gPatchFields @be @x

instance GPatchFields be x => GPatchFields be (C1 f x) where
  gPatchFields = M1 $ gPatchFields @be @x

instance (GPatchFields be x, GPatchFields be y) => GPatchFields be (x :*: y) where
  gPatchFields = gPatchFields @be @x :*: gPatchFields @be @y

instance BackendFromField be a => GPatchFields be (S1 f (K1 Generic.R (Exposed a))) where
  gPatchFields = M1 $ K1 FieldDecoder

instance ( GPatchFields be (Rep (tbl Exposed))
         , FieldPatches be (Rep (tbl Exposed)) ~ Rep (tbl (FieldDecoder be))
         , Generic (tbl (FieldDecoder be))
         ) => GPatchFields be (S1 f (K1 Generic.R (tbl Exposed))) where
  gPatchFields = M1 $ K1 $ to' $ gPatchFields @be @(Rep (tbl Exposed))

instance ( GPatchFields be (Rep (tbl (Nullable Exposed)))
         , FieldPatches be (Rep (tbl (Nullable Exposed))) ~ Rep (tbl (Nullable (FieldDecoder be)))
         , Generic (tbl (Nullable (FieldDecoder be)))
         ) => GPatchFields be (S1 f (K1 Generic.R (tbl (Nullable Exposed)))) where
  gPatchFields = M1 $ K1 $ to' $ gPatchFields @be @(Rep (tbl (Nullable Exposed)))

class PatchEntity be e where
  patchEntity :: EntityPatchDecoder be e

to' :: Generic x => Rep x () -> x
to' = Generic.to

instance ( Generic (tbl (FieldDecoder be))
         , GPatchFields be (Rep (tbl Exposed))
         , FieldPatches be (Rep (tbl Exposed)) ~ Rep (tbl (FieldDecoder be))
         , Table tbl
         , Ord (PrimaryKey tbl Identity)
         , Eq (tbl Maybe)
         ) => PatchEntity be (TableEntity tbl) where
  patchEntity = EntityPatchDecoder_Table $ to' $ gPatchFields @be @(Rep (tbl Exposed))

defaultDatabasePatchDecoder
  :: forall be db
  .  ( Generic (db (EntityPatchDecoder be))
     , GPatchDatabase (Rep (db (EntityPatchDecoder be)) ())
     )
  => db (EntityPatchDecoder be)
defaultDatabasePatchDecoder = to' gDatabasePatchDecoder
