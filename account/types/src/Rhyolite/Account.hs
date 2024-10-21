{-|
Description:
  Default email-based account type
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Rhyolite.Account where

import Prelude hiding ((.))

import Control.Category
import Control.Lens (iso)
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.Constraint.Extras.TH
import Data.Functor.Identity
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Int (Int64)
import Data.Kind (Constraint)
import Data.Signed
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable
import Data.UUID
import Database.Beam.Backend.SQL.Types
import Database.Beam.Schema
import GHC.Generics hiding (R)
import Obelisk.Common.Schema
import Obelisk.Route
import Obelisk.Route.TH

-- TODO: Unique constraint on email
-- | The Account table defines user identities and how to authenticate them
data Account f = Account
  { _account_id :: Columnar f UUID
  , _account_email :: Columnar f Text
  , _account_password :: Columnar f (Maybe ByteString)
  , _account_passwordResetNonce :: Columnar f (Maybe UTCTime)
  } deriving (Generic)

deriving instance AllFieldsHave Show (Account f) => Show (Account f)
deriving instance AllFieldsHave Eq (Account f) => Eq (Account f)
deriving instance AllFieldsHave Ord (Account f) => Ord (Account f)

instance Beamable Account

instance Table Account where
  newtype PrimaryKey Account f = AccountId
    { _accountId_id :: Columnar f UUID
    }
    deriving (Generic)
  primaryKey = AccountId . _account_id

instance Beamable (PrimaryKey Account)

deriving instance AllFieldsHave ToJSON (PrimaryKey Account f) => ToJSON (PrimaryKey Account f)
deriving instance AllFieldsHave FromJSON (PrimaryKey Account f) => FromJSON (PrimaryKey Account f)
deriving instance AllFieldsHave ToJSONKey (PrimaryKey Account f) => ToJSONKey (PrimaryKey Account f)
deriving instance AllFieldsHave FromJSONKey (PrimaryKey Account f) => FromJSONKey (PrimaryKey Account f)

deriving instance AllFieldsHave Eq (PrimaryKey Account f) => Eq (PrimaryKey Account f)
deriving instance AllFieldsHave Ord (PrimaryKey Account f) => Ord (PrimaryKey Account f)
deriving instance AllFieldsHave Show (PrimaryKey Account f) => Show (PrimaryKey Account f)

newtype PasswordResetToken = PasswordResetToken
  { unPasswordResetToken :: (PrimaryKey Account Identity, UTCTime)
  }
  deriving (Generic)

instance ToJSON PasswordResetToken
instance FromJSON PasswordResetToken

data AccountRoute next where
  AccountRoute_Login :: AccountRoute ()
  AccountRoute_CreateAccount :: AccountRoute ()
  AccountRoute_FinishAccountCreation :: AccountRoute (Signed PasswordResetToken)
  AccountRoute_ForgotPassword :: AccountRoute ()
  AccountRoute_ResetPassword :: AccountRoute (Signed PasswordResetToken)

--TODO: Move somewhere better
signedEncoder :: (MonadError Text parse, Applicative check) => Encoder check parse (Signed a) PageName
signedEncoder = singlePathSegmentEncoder . viewEncoder (iso unSigned Signed)

defaultAccountRouteEncoder
  :: (MonadError Text check, MonadError Text parse)
  => Encoder check parse (R AccountRoute) PageName
defaultAccountRouteEncoder = pathComponentEncoder $ \case
  AccountRoute_Login -> PathSegment "login" $ unitEncoder mempty
  AccountRoute_CreateAccount -> PathSegment "create" $ unitEncoder mempty
  AccountRoute_FinishAccountCreation -> PathSegment "finish" signedEncoder
  AccountRoute_ForgotPassword -> PathSegment "forgot" $ unitEncoder mempty
  AccountRoute_ResetPassword -> PathSegment "reset" signedEncoder

newtype AuthToken = AuthToken { unAuthToken :: PrimaryKey Account Identity }
  deriving stock (Show, Eq, Ord, Typeable)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

type Email = Text

type Password = Text

data FinishAccountCreationError
   = FinishAccountCreationError_InvalidToken
   deriving stock (Show, Read, Eq, Ord, Generic)

instance ToJSON FinishAccountCreationError
instance FromJSON FinishAccountCreationError
instance ToJSONKey FinishAccountCreationError
instance FromJSONKey FinishAccountCreationError

data AccountRequest result where
  AccountRequest_Login
    :: Email
    -> Password
    -> AccountRequest (Maybe (Signed AuthToken))
  AccountRequest_CreateAccount
    :: Email
    -> AccountRequest ()
  AccountRequest_FinishAccountCreation
    :: Signed PasswordResetToken
    -> Password
    -> AccountRequest (Either FinishAccountCreationError (Signed AuthToken))
  AccountRequest_ForgotPassword
    :: Email
    -> AccountRequest ()
  AccountRequest_ResetPassword
    :: Signed PasswordResetToken
    -> Password
    -> AccountRequest (Either FinishAccountCreationError (Signed AuthToken))

deriving stock instance Show (AccountRequest result)

concat <$> sequence
  [ deriveRouteComponent ''AccountRoute
  , deriveJSONGADT ''AccountRequest
  , deriveArgDict ''AccountRequest
  , deriveGShow ''AccountRequest
  , deriveGEq ''AccountRequest
  , deriveGCompare ''AccountRequest
  ]
