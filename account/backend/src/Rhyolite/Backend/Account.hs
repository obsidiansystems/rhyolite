{-# Language FlexibleContexts #-}
{-# Language MonoLocalBinds #-}
{-# Language OverloadedStrings #-}
module Rhyolite.Backend.Account
  ( createAccount
  , login
  ) where

import Control.Monad (guard)
import Control.Monad.Trans.Maybe
import Crypto.PasswordStore
import Data.Aeson
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Functor.Identity
import Data.Maybe
import Data.Text
import qualified Data.Text.Encoding as T
import Data.Time
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Postgres
import Database.Beam.Postgres.Full hiding (insert)
import Database.Beam.Postgres.Syntax
import Database.Beam.Postgres.Syntax
import Database.Beam.Schema
import Database.PostgreSQL.Simple.Beam
import Rhyolite.Account
import Rhyolite.DB.NotifyListen

-- | Postgres @current_timestamp()@ function. Returns the server's timestamp
current_timestamp_ :: QExpr Postgres s UTCTime
current_timestamp_ = QExpr (\_ -> PgExpressionSyntax (emit "current_timestamp at time zone 'UTC'"))

-- | Creates a new account and emits a db notification about it
createAccount
  :: (Has' ToJSON notice Identity, ForallF ToJSON notice)
  => DatabaseEntity Postgres db (TableEntity Account)
  -> notice (PrimaryKey Account Identity)
  -> Text
  -> Text
  -> Pg (Either Text (PrimaryKey Account Identity))
createAccount accountTable noticeWrapper email pass = do
  salt <- liftIO genSaltIO
  let hash = makePasswordSaltWith pbkdf2 (2^) (T.encodeUtf8 pass) salt 14
  accountIds <- runPgInsertReturningList $ flip returning _account_id $ insert accountTable $ insertExpressions
    [ Account
        { _account_id = default_
        , _account_email = val_ email
        , _account_password = val_ (Just hash)
        , _account_passwordResetNonce = just_ current_timestamp_
        }
    ]
  case accountIds of
    [accountId] -> do
      notify NotificationType_Insert noticeWrapper (AccountId accountId)
      pure $ Right $ AccountId accountId
    _ -> pure $ Left "Failed to create account"

-- | Attempts to login a user given some credentials.
login
  :: Database Postgres db
  => DatabaseEntity Postgres db (TableEntity Account)
  -> Text
  -> Text
  -> Pg (Maybe (PrimaryKey Account Identity))
login accountTable email pass = runMaybeT $ do
  (aid, mPwHash) <- MaybeT $ fmap listToMaybe $ runSelectReturningList $ select $ do
    acc <- all_ accountTable
    guard_ $ _account_email acc ==. val_ email
    pure (_account_id acc, _account_password acc)
  pwHash <- MaybeT $ pure mPwHash
  guard $ verifyPasswordWith pbkdf2 (2^) (T.encodeUtf8 pass) pwHash
  pure (AccountId aid)
