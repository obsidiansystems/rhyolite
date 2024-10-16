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

type HasAccountConstraint (c :: * -> Constraint) f =
  ( c (Columnar f (SqlSerial Int64))
  , c (Columnar f Text)
  , c (Columnar f (Maybe ByteString))
  , c (Columnar f (Maybe UTCTime))
  )

deriving instance HasAccountConstraint Eq f => Eq (Account f)
deriving instance HasAccountConstraint Ord f => Ord (Account f)

instance Beamable Account

instance Table Account where
  newtype PrimaryKey Account f = AccountId
    { _accountId_id :: Columnar f (SqlSerial Int64)
    }
    deriving (Generic)
  primaryKey = AccountId . _account_id

instance Beamable (PrimaryKey Account)

type AccountId = PrimaryKey Account Identity

type HasAccountIdConstraint (c :: * -> Constraint) f = c (Columnar f (SqlSerial Int64))

deriving instance HasAccountIdConstraint ToJSON f => ToJSON (PrimaryKey Account f)
deriving instance HasAccountIdConstraint FromJSON f => FromJSON (PrimaryKey Account f)
deriving instance HasAccountIdConstraint ToJSONKey f => ToJSONKey (PrimaryKey Account f)
deriving instance HasAccountIdConstraint FromJSONKey f => FromJSONKey (PrimaryKey Account f)

deriving instance HasAccountIdConstraint Eq f => Eq (PrimaryKey Account f)
deriving instance HasAccountIdConstraint Ord f => Ord (PrimaryKey Account f)
deriving instance HasAccountIdConstraint Show f => Show (PrimaryKey Account f)

newtype PasswordResetToken = PasswordResetToken
  { unPasswordResetToken :: (PrimaryKey Account Identity, UTCTime)
  }
  deriving (Generic)

instance ToJSON PasswordResetToken
instance FromJSON PasswordResetToken

