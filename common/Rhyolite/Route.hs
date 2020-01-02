-- | This module is still here for backward compatibility reasons. You should
-- use "Obelisk.Route" instead.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rhyolite.Route where

import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Base
#if defined(VERSION_monad_logger)
import Control.Monad.Logger
#endif
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Default
import qualified Data.Text as T
import Data.Text.Encoding
import Network.URI
import Network.HTTP.Types.URI (renderQuery, parseQuery)
import qualified Data.Map as Map
import Data.Map (Map)

import Rhyolite.Request.Common (decodeValue')

class Monad m => MonadRoute r m | m -> r where
  routeToUrl :: r -> m URI

type RouteEnv = (String, String, String) -- (protocol, hostname, anything after hostname (e.g., port))

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

runRouteT :: RouteT r m a -> RouteEnv -> m a
runRouteT = runReaderT . unRouteT

instance (Monad m, ToJSON r, Default r, Eq r) => MonadRoute r (RouteT r m) where
  routeToUrl r = do
    routeEnv <- RouteT ask
    return $ routeToUrlDefault routeEnv r

routeToUrlDefault :: (ToJSON r, Default r, Eq r)
                  => RouteEnv
                  -> r
                  -> URI
routeToUrlDefault (baseProto, baseHost, basePort) r =
  let base = URI baseProto (Just $ URIAuth "" baseHost basePort) "/"
  in base (routeToQuery r) "" --TODO: https

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

newtype SubRouteT r r' m a = SubRouteT (ReaderT (r' -> r) m a) deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadRoute r m) => MonadRoute r' (SubRouteT r r' m) where
  routeToUrl r = SubRouteT $ do
    routeConv <- ask
    lift $ routeToUrl $ routeConv r

instance MonadTrans (SubRouteT r r') where
  lift = SubRouteT . lift

runSubRouteT :: SubRouteT r r' m a -> (r' -> r) -> m a
runSubRouteT (SubRouteT a) = runReaderT a

routeToSubdomainUrl :: (MonadRoute r m) => String -> r -> m URI
routeToSubdomainUrl sub x = fmap (addSubdomain sub) (routeToUrl x)

addSubdomain :: String -> URI -> URI
addSubdomain sub uri = uri { uriAuthority = fmap (\auth -> auth { uriRegName = sub ++ "." ++ uriRegName auth }) (uriAuthority uri) }

getDefaultParam :: FromJSON b => Map BS.ByteString (Maybe BS.ByteString) -> Maybe b
getDefaultParam params = do
  Just v <- Map.lookup (encodeUtf8 "x") params
  decodeValue' (LBS.fromStrict v)

decodeRoute :: (FromJSON r) => T.Text -> Maybe r
decodeRoute t = do
  Just v <- Map.lookup (encodeUtf8 "x") (Map.fromList (parseQuery (encodeUtf8 t)))
  decodeValue' (LBS.fromStrict v)

uriToRouteEnv
  :: URI
  -> Maybe RouteEnv
uriToRouteEnv u = do
  let s = uriScheme u
  a <- uriAuthority u
  return (s, uriUserInfo a <> uriRegName a, uriPort a <> uriPath u <> uriQuery u <> uriFragment u)
