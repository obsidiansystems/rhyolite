{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Wrap non-Beam-style higher-kinded records in a Beam-friendly newtype
--
-- Beam requires table fields to be wrapped in "Columnar", which makes them
-- harder to deal with in many ways.  To avoid polluting our entire codebase
-- with Columnar, we can use this wrapper which enables non-Columnar-wrapped
-- HKDs to be used with Beam operations like `select`.
module Obelisk.Beam.BeamWrap where

import Data.Proxy
import Database.Beam
import Database.Beam.Backend.SQL.Row
import Database.Beam.Backend.Types
import Database.Beam.Query.Internal
import Database.Beam.Schema.Tables
import GHC.Generics
import Obelisk.Beam.DZippable

-- | Wrap non-Beam-style higher-kinded records in a Beam-friendly newtype
--
-- Beam requires table fields to be wrapped in "Columnar", which makes them
-- harder to deal with in many ways.  To avoid polluting our entire codebase
-- with Columnar, we can use this wrapper which enables non-Columnar-wrapped
-- HKDs to be used with Beam operations like `select`.
--
-- Example:
--
-- > data MyRecord f = MyRecord { myField :: f Bool }
-- >
-- > fmap (fmap unBeamWrap) $ runSelectReturningList $ select $ pure $ MyRecord { myField = val_ True }
--
-- Passing `MyRecord` through without BeamWrap would result in a missing instance.
--
-- NOTE: The type arguments to BeamWrap are in a silly order to avoid
-- overlapping with instances in Beam
newtype BeamWrap f t = BeamWrap { unBeamWrap :: t f }
  deriving stock (Generic)

type instance QExprToIdentity (BeamWrap (QGenExpr context syntax s) t) = BeamWrap Identity t

-- NOTE: It would be better to have:
-- deriving newtype instance FromBackendRow be (t f) => FromBackendRow be (BeamWrap f t)
-- so that underlying types could define their own FromBackendRow.  However,
-- those instances would always overlap with one of the instances in Beam, so
-- instead we define it here.  This is no less general than what Beam provides.
instance (BeamBackend be, Generic (t f), GFromBackendRow2 be (Rep (t f))) => FromBackendRow be (BeamWrap f t) where
  fromBackendRow = BeamWrap . to <$> gFromBackendRow2
  valuesNeeded _ _ = gValuesNeeded2 @be @(Rep (t f))

class GFromBackendRow2 be rep where
  gFromBackendRow2 :: FromBackendRowM be (rep ())
  gValuesNeeded2 :: Int

instance FromBackendRow be a => GFromBackendRow2 be (K1 i a) where
  gFromBackendRow2 = K1 <$> fromBackendRow
  gValuesNeeded2 = valuesNeeded (Proxy @be) (Proxy @a)

instance FromBackendRow be a => GFromBackendRow2 be U1 where
  gFromBackendRow2 = pure U1
  gValuesNeeded2 = 0

instance (GFromBackendRow2 be a, GFromBackendRow2 be b) => GFromBackendRow2 be (a :*: b) where
  gFromBackendRow2 = (:*:) <$> gFromBackendRow2 <*> gFromBackendRow2
  gValuesNeeded2 = gValuesNeeded2 @be @a + gValuesNeeded2 @be @b

instance GFromBackendRow2 be a => GFromBackendRow2 be (M1 i t a) where
  gFromBackendRow2 = M1 <$> gFromBackendRow2
  gValuesNeeded2 = gValuesNeeded2 @be @a

instance (DMappable t, DPointed t) => ProjectibleWithPredicate AnyType be (WithExprContext (BeamSqlBackendExpressionSyntax' be)) (BeamWrap (QGenExpr context be s) t) where
  project' _ _ mutateM (BeamWrap a) = BeamWrap <$> dmap (\(QExpr e) -> QExpr . fmap fromBeamSqlBackendExpressionSyntax <$> mutateM Proxy Proxy (BeamSqlBackendExpressionSyntax' . e)) a
  projectSkeleton' _ _ mkM = BeamWrap <$> dpure (QExpr . fmap fromBeamSqlBackendExpressionSyntax <$> mkM Proxy Proxy)
