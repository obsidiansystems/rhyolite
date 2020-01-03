-- | In this module we have a notion of 'Account' with its associated route ('AccountRoute'), and encoder ('accountRouteEncoder').

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rhyolite.Account where

import Control.Lens(Iso, iso)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Database.Id.Class (HasId, Id)
import GHC.Generics (Generic)

import Rhyolite.Schema (Email)
import Obelisk.Route(Encoder, isoEncoder)
import Rhyolite.Sign (Signed(..))


data Account = Account
  { account_email :: Email
  , account_passwordHash :: Maybe ByteString
  , account_passwordResetNonce :: Maybe UTCTime
  } deriving (Show, Read, Eq, Ord, Typeable)

instance HasId Account

newtype PasswordResetToken f = PasswordResetToken { unPasswordResetToken :: (f (Id Account), UTCTime) }

newtype AuthToken f = AuthToken { unAuthToken :: f (Id Account) } deriving (Typeable)

deriving instance (Show (f (Id Account))) => Show (AuthToken f)
deriving instance (Read (f (Id Account))) => Read (AuthToken f)
deriving instance (Eq (f (Id Account))) => Eq (AuthToken f)
deriving instance (Ord (f (Id Account))) => Ord (AuthToken f)
deriving instance (ToJSON (f (Id Account))) => ToJSON (AuthToken f)
deriving instance (FromJSON (f (Id Account))) => FromJSON (AuthToken f)

deriving instance (Show (f (Id Account))) => Show (PasswordResetToken f)
deriving instance (Read (f (Id Account))) => Read (PasswordResetToken f)
deriving instance (Eq (f (Id Account))) => Eq (PasswordResetToken f)
deriving instance (Ord (f (Id Account))) => Ord (PasswordResetToken f)
deriving instance (ToJSON (f (Id Account))) => ToJSON (PasswordResetToken f)
deriving instance (FromJSON (f (Id Account))) => FromJSON (PasswordResetToken f)

newtype AccountRoute f = AccountRoute_PasswordReset { unAccountRoute :: Signed (PasswordResetToken f) } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON (AccountRoute f)
instance FromJSON (AccountRoute f)

_AccountRoute :: Iso (AccountRoute f) (AccountRoute g) (Signed (PasswordResetToken f)) (Signed (PasswordResetToken g))
_AccountRoute = iso unAccountRoute AccountRoute_PasswordReset
_Signed :: Iso (Signed (PasswordResetToken f)) (Signed (PasswordResetToken g)) Text Text
_Signed = iso unSigned Signed

accountRouteEncoder :: (Applicative check, Applicative parse) => Encoder check parse (AccountRoute f) Text
accountRouteEncoder = isoEncoder (_AccountRoute . _Signed)
