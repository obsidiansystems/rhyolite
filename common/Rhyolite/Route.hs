{-|
Description: Legacy application routes

This module is still here for backward compatibility reasons. You should use
"Obelisk.Route" instead.
-}
{-# Language CPP #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}

module Rhyolite.Route where

import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
#if defined(VERSION_monad_logger)
import Control.Monad.Logger
#endif
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Types.URI (parseQuery, renderQuery)
import Network.URI

-- | A class that can produce application-specific URLs
class Monad m => MonadRoute r m | m -> r where
  routeToUrl :: r -> m URI

-- | The protocol, hostname, and rest (e.g., port) of the app's configured route
type RouteEnv = (String, String, String)

-- | A transformer that carries around information about the route
newtype RouteT r m a = RouteT { unRouteT :: ReaderT RouteEnv m a }
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadTrans
#if defined(VERSION_monad_logger)
    , MonadLogger
#endif
    )

instance MonadTransControl (RouteT r) where
    type StT (RouteT r) a = a
    liftWith f = RouteT . ReaderT $ \r -> f $ \t -> runRouteT t r
    restoreT = RouteT . ReaderT . const
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadBase b m => MonadBase b (RouteT r m) where
  liftBase = lift . liftBase

instance (MonadBaseControl b m) => MonadBaseControl b (RouteT r m) where
  type StM (RouteT r m) a = ComposeSt (RouteT r) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

-- | Runs a 'RouteT' action
runRouteT :: RouteT r m a -> RouteEnv -> m a
runRouteT = runReaderT . unRouteT

instance (Monad m, ToJSON r, Default r, Eq r) => MonadRoute r (RouteT r m) where
  routeToUrl r = do
    routeEnv <- RouteT ask
    return $ routeToUrlDefault routeEnv r

-- | Constructs a route using a 'RouteEnv'
routeToUrlDefault :: (ToJSON r, Default r, Eq r)
                  => RouteEnv
                  -> r
                  -> URI
routeToUrlDefault (baseProto, baseHost, basePort) r =
  let base = URI baseProto (Just $ URIAuth "" baseHost basePort) "/"
  in base (routeToQuery r) "" --TODO: https

-- | Constructs a query string containing json-encoded route information.
-- Seriously, use 'Obelisk.Route' instead.
routeToQuery :: (ToJSON r, Default r, Eq r) => r -> String
routeToQuery r = if r == def
  then ""
  else T.unpack . decodeUtf8 $ renderQuery True [("x", Just $ LBS.toStrict $ encode r)]

instance MonadRoute r m => MonadRoute r (ReaderT a m) where
  routeToUrl r = lift $ routeToUrl r

instance MonadRoute r m => MonadRoute r (MaybeT m) where
  routeToUrl r = lift $ routeToUrl r

instance MonadRoute r m => MonadRoute r (ExceptT a m) where
  routeToUrl r = lift $ routeToUrl r

-- | A transformer that can produce route information for a subset of routes
newtype SubRouteT r r' m a = SubRouteT (ReaderT (r' -> r) m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadRoute r m) => MonadRoute r' (SubRouteT r r' m) where
  routeToUrl r = SubRouteT $ do
    routeConv <- ask
    lift $ routeToUrl $ routeConv r

instance MonadTrans (SubRouteT r r') where
  lift = SubRouteT . lift

-- | Runs a 'SubRouteT' action
runSubRouteT :: SubRouteT r r' m a -> (r' -> r) -> m a
runSubRouteT (SubRouteT a) = runReaderT a

-- | Produce urls for a subdomain
routeToSubdomainUrl :: (MonadRoute r m) => String -> r -> m URI
routeToSubdomainUrl sub x = fmap (addSubdomain sub) (routeToUrl x)

-- | Tacks a subdomain onto a given 'URI"
addSubdomain :: String -> URI -> URI
addSubdomain sub uri = uri
  { uriAuthority = fmap
      (\auth -> auth { uriRegName = sub ++ "." ++ uriRegName auth })
      (uriAuthority uri)
  }

-- | Get the json-encoded query parameter used by this module to store and
-- convey route infromation
getDefaultParam :: FromJSON b => Map BS.ByteString (Maybe BS.ByteString) -> Maybe b
getDefaultParam params = do
  Just v <- Map.lookup (encodeUtf8 "x") params
  decodeStrict' v

-- | JSON-decode route information
decodeRoute :: (FromJSON r) => T.Text -> Maybe r
decodeRoute t = do
  Just v <- Map.lookup (encodeUtf8 "x") (Map.fromList (parseQuery (encodeUtf8 t)))
  decodeStrict' v

-- | Constructs a 'RouteEnv' object from a 'URI'.
uriToRouteEnv
  :: URI
  -> Maybe RouteEnv
uriToRouteEnv u = do
  let s = uriScheme u
  a <- uriAuthority u
  return (s, uriUserInfo a <> uriRegName a, uriPort a <> uriPath u <> uriQuery u <> uriFragment u)
