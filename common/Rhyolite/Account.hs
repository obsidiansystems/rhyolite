{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Rhyolite.Account where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Rhyolite.Schema (HasId, Email, Id)
import Rhyolite.Sign (Signed)
import Rhyolite.Request.TH (makeJson)


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

newtype AccountRoute f = AccountRoute_PasswordReset (Signed (PasswordResetToken f)) deriving (Show, Read, Eq, Ord)

data LoginError
  = LoginError_UserNotFound
  | LoginError_InvalidPassword
  deriving (Eq, Ord, Read, Generic)
instance FromJSON LoginError
instance ToJSON LoginError

instance Show LoginError where
  show LoginError_UserNotFound = "The user is not recognized"
  show LoginError_InvalidPassword = "Please enter a valid password"

makeJson ''AccountRoute
