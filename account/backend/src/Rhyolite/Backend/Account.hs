{-|
Description:
  Check or modify credentials
-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language MonoLocalBinds #-}
{-# Language OverloadedStrings #-}
module Rhyolite.Backend.Account
  ( createAccount
  , ensureAccountExists
  , setAccountPassword
  , setAccountPasswordHash
  , makePasswordHash
  , passwordResetToken
  , newNonce
  , resetPassword
  , resetPasswordHash
  ) where

import Rhyolite.Backend.Account.Db hiding (createAccount, ensureAccountExists)
import Rhyolite.Backend.Account.Notify (createAccount, ensureAccountExists)
