{-# LANGUAGE RankNTypes #-}

module Rhyolite.Backend.Vessel where

import Rhyolite.Account
import Rhyolite.Schema
import Rhyolite.Sign
import Data.TSum
import Data.Vessel
import Control.Monad.IO.Class
import Data.GADT.Compare
import Data.Functor.Identity
import Data.Functor.Const
import qualified Rhyolite.Map.Monoidal as Map
import Data.IORef

-- TODO: Move these functions somewhere sensible.
buildAuthTokenV :: forall pub priv m a. (MonadIO m, MonadSign m, GCompare pub, GCompare priv)
  => Vessel (TSum (Signed (AuthToken Identity)) pub priv) Const a -- ^ View selector
  -> (forall x. pub x -> m (Maybe x)) -- ^ Handler for public queries
  -> (forall x. Id Account -> priv x -> m (Maybe x)) -- ^ Handler for private queries
  -> m (Vessel (TSum (Signed (AuthToken Identity)) pub priv) (,) a)
buildAuthTokenV vs publicHandler privateHandler = do
  cache <- liftIO (newIORef Map.empty) -- We cache the results of readSigned just to avoid repeatedly decrypting the same strings.
  let checkToken t = do
        m <- liftIO (readIORef cache)
        case Map.lookup t m of
          Nothing -> do
            v <- readSigned t
            liftIO (writeIORef cache (Map.insert t v m))
            return v
          Just v -> return v
  buildTSumV vs checkToken publicHandler (\(AuthToken (Identity aid)) -> privateHandler aid)

buildAuthTokenV' :: forall priv m a. (MonadIO m, MonadSign m, GCompare priv)
  => Vessel (TSum (Signed (AuthToken Identity)) Empty priv) Const a -- ^ View selector
  -> (forall x. Id Account -> priv x -> m (Maybe x)) -- ^ Handler for private queries
  -> m (Vessel (TSum (Signed (AuthToken Identity)) Empty priv) (,) a)
buildAuthTokenV' vs privateHandler = buildAuthTokenV vs (\_ -> return Nothing) privateHandler