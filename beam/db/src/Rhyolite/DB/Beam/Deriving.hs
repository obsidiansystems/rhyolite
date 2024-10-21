{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

{-|
   Description:
     Types and helpers for Beam that can be used in schemas and database interactions.
-}
module Rhyolite.DB.Beam.Deriving where

import Control.Monad
import Data.Int (Int64)
import Data.Proxy
import Data.Text (Text)
import Database.Beam
import Database.Beam.AutoMigrate
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Text.Read (readEither)

----

newtype ReadShowColumn a = ReadShowColumn { unReadShowColumn :: a }

instance HasColumnType (ReadShowColumn a) where
  defaultColumnType _ = defaultColumnType $ Proxy @Text
instance (HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be (ReadShowColumn a) where
  sqlEqE _ = sqlEqE $ Proxy @Text
  sqlNeqE _ = sqlNeqE $ Proxy @Text
  sqlEqTriE _ = sqlEqTriE $ Proxy @Text
  sqlNeqTriE _ = sqlNeqTriE $ Proxy @Text
instance Show a => HasSqlValueSyntax PgValueSyntax (ReadShowColumn a) where
  sqlValueSyntax (ReadShowColumn x) = sqlValueSyntax $ show x
instance (Typeable a, Read a) => FromField (ReadShowColumn a) where
  fromField f d = fromField @String f d >>=
    either (returnError ConversionFailed f) (pure . ReadShowColumn) . readEither
instance Show a => ToField (ReadShowColumn a) where
  toField a = toField (show $ unReadShowColumn a)

----

newtype EnumColumn a = EnumColumn { unEnumColumn :: a }

instance HasColumnType (EnumColumn a) where
  defaultColumnType _ = defaultColumnType $ Proxy @Int64
instance Enum a => HasSqlValueSyntax PgValueSyntax (EnumColumn a) where
  sqlValueSyntax (EnumColumn x) = sqlValueSyntax $ toEnum @Int64 $ fromEnum x
instance (Enum a, Bounded a) => FromField (EnumColumn a) where
  fromField f d = fromField @Int64 f d >>= \x -> do
    let minBound', maxBound' :: Int64
        minBound' = toEnum $ fromEnum (minBound :: a)
        maxBound' = toEnum $ fromEnum (maxBound :: a)
    when (x < minBound') $ returnError ConversionFailed f "underflow"
    when (x > maxBound') $ returnError ConversionFailed f "overflow"
    pure $ EnumColumn $ toEnum $ fromEnum x
