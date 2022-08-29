{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Beam.Patch.Row
  ( RowPatch (..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid.DecidablyEmpty
import Database.Beam.Schema.Tables
import Data.Patch
import Control.Monad.Writer.CPS (runWriter, tell)
import Data.Monoid (Any(..))

-- | Represents a patch that updates a row by replacing some of its columns' values.  This is what Postgres actually gives us via Logical Decoding
newtype RowPatch tbl = RowPatch { unRowPatch :: tbl Maybe }

deriving instance ToJSON (tbl Maybe) => ToJSON (RowPatch tbl)
deriving instance FromJSON (tbl Maybe) => FromJSON (RowPatch tbl)

instance Beamable tbl => Semigroup (RowPatch tbl) where
  RowPatch a <> RowPatch b = RowPatch $ runIdentity $ zipBeamFieldsM (\(Columnar' x) (Columnar' y) -> pure $ Columnar' $ x <|> y) a b

instance Beamable tbl => Monoid (RowPatch tbl) where
  mempty = RowPatch $ runIdentity $ zipBeamFieldsM (\_ _ -> pure $ Columnar' Nothing) tblSkeleton tblSkeleton
  mappend = (<>)

instance (Eq (tbl Maybe), Beamable tbl) => DecidablyEmpty (RowPatch tbl)

deriving instance Show (tbl Maybe) => Show (RowPatch tbl)
deriving instance Eq (tbl Maybe) => Eq (RowPatch tbl)

instance Beamable tbl => Patch (RowPatch tbl) where
  type PatchTarget (RowPatch tbl) = tbl Identity
  apply (RowPatch new) old = if didPatch then Just patchResult else Nothing
    where
      (patchResult, Any didPatch) = runWriter $ zipBeamFieldsM go new old
      go (Columnar' newField) (Columnar' oldField) = Columnar' (fromMaybe oldField newField) <$ tell (Any $ isJust newField)
