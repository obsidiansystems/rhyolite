{-|
Description:
  Default email-based account type
-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}
{-# Language ConstraintKinds #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Rhyolite.Account where

import Data.Kind (Constraint)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Beam.Backend.SQL.Types
import Database.Beam.Schema
import GHC.Generics

-- | The Account table defines user identities and how to authenticate them
data Account f = Account
  { _account_id :: Columnar f (SqlSerial Int64)
  , _account_email :: Columnar f Text
  , _account_password :: Columnar f (Maybe ByteString)
  , _account_passwordResetNonce :: Columnar f (Maybe UTCTime)
  } deriving (Generic)

type HasAccountConstrant (c :: * -> Constraint) f =
  ( c (Columnar f (SqlSerial Int64))
  , c (Columnar f Text)
  , c (Columnar f (Maybe ByteString))
  , c (Columnar f (Maybe UTCTime))
  )

deriving instance HasAccountConstrant Eq f => Eq (Account f)
deriving instance HasAccountConstrant Ord f => Ord (Account f)

instance Beamable Account

instance Table Account where
  newtype PrimaryKey Account f = AccountId
    { _accountId_id :: Columnar f (SqlSerial Int64)
    }
    deriving (Generic)
  primaryKey = AccountId . _account_id

instance Beamable (PrimaryKey Account)

type HasAccountIdConstrant (c :: * -> Constraint) f = c (Columnar f (SqlSerial Int64))

deriving instance HasAccountIdConstrant ToJSON f => ToJSON (PrimaryKey Account f)
deriving instance HasAccountIdConstrant FromJSON f => FromJSON (PrimaryKey Account f)
deriving instance HasAccountIdConstrant ToJSONKey f => ToJSONKey (PrimaryKey Account f)
deriving instance HasAccountIdConstrant FromJSONKey f => FromJSONKey (PrimaryKey Account f)

deriving instance HasAccountIdConstrant Eq f => Eq (PrimaryKey Account f)
deriving instance HasAccountIdConstrant Ord f => Ord (PrimaryKey Account f)
deriving instance HasAccountIdConstrant Show f => Show (PrimaryKey Account f)

newtype PasswordResetToken = PasswordResetToken
  { unPasswordResetToken :: (PrimaryKey Account Identity, UTCTime)
  }
  deriving (Generic)

instance ToJSON PasswordResetToken
instance FromJSON PasswordResetToken

