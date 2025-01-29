{-|
Description:
  Create or modify accounts in the database, and send LISTEN notifications
-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language MonoLocalBinds #-}
{-# Language OverloadedStrings #-}
module Rhyolite.Backend.Account.Notify
  ( createAccount
  , login
  , ensureAccountExists
  , setAccountPassword
  , setAccountPasswordHash
  , makePasswordHash
  , passwordResetToken
  , newNonce
  , resetPassword
  , resetPasswordHash
  ) where

import Data.Aeson
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Functor.Identity
import Data.Text
import Database.Beam
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.Beam ()
import Rhyolite.Account
import Rhyolite.Backend.Account.Db hiding (createAccount, ensureAccountExists)
import qualified Rhyolite.Backend.Account.Db as Acc
import Rhyolite.DB.NotifyListen
import Rhyolite.DB.NotifyListen.Beam

-- | Creates a new account and emits a db notification about it
createAccount
  :: (Has' ToJSON notice Identity, ForallF ToJSON notice)
  => DatabaseEntity Postgres db (TableEntity Account)
  -> notice (PrimaryKey Account Identity)
  -> Text
  -> Text
  -> Pg (Either Text (PrimaryKey Account Identity))
createAccount accountTable noticeWrapper email pass = do
  result <- Acc.createAccount accountTable email pass
  case result of
    Right accountId ->
      notify NotificationType_Insert noticeWrapper accountId
    _ -> pure ()
  pure result

ensureAccountExists
  :: (Database Postgres db, HasNotification n Account, Has' ToJSON n Identity, ForallF ToJSON n)
  => DatabaseEntity Postgres db (TableEntity Account)
  -> Text
  -> Pg (Bool, PrimaryKey Account Identity)
ensureAccountExists accountTable email =  do
  aid <- Acc.ensureAccountExists accountTable email
  notify NotificationType_Insert (notification accountTable) $ snd aid
  pure aid
