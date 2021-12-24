{-|
Description:
  Default email-based account type
-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeFamilies #-}
module Rhyolite.Account where

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

instance Beamable Account

instance Table Account where
  newtype PrimaryKey Account f = AccountId
    { _accountId_id :: Columnar f (SqlSerial Int64)
    }
    deriving (Generic)
  primaryKey = AccountId . _account_id

instance Beamable (PrimaryKey Account)

deriving instance Eq (PrimaryKey Account Identity)
deriving instance Ord (PrimaryKey Account Identity)
deriving instance Show (PrimaryKey Account Identity)

instance ToJSON (PrimaryKey Account Identity)
instance FromJSON (PrimaryKey Account Identity)
