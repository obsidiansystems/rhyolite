-- | Frontend part of the view/viewselector implementation. Here we have the
-- definition of 'RhyoliteWidget' and the functions to run it,
-- 'runRhyoliteWidget' and 'runObeliskRhyoliteWidget'. We also have the
-- 'watchViewSelector' function that's used in the frontend module of a typical
-- app.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rhyolite.Frontend.App where

import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Bifunctor
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import Data.Constraint.Extras
import Data.Default (Default)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Some
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Obelisk.Route.Frontend (Routed(..), SetRoute(..), RouteToUrl(..))
import Network.URI (URI, parseURI)
import qualified Reflex as R
import Data.Witherable (Filterable)
import Reflex.Dom.Core hiding (MonadWidget, Request)
import Reflex.Host.Class
import Reflex.Time (throttleBatchWithLag)

import Rhyolite.Api
import Rhyolite.App
import Rhyolite.WebSocket

import Obelisk.Configs
import Obelisk.Route hiding (Decoder)
import Obelisk.Route.Frontend hiding (Decoder)

#if defined(ghcjs_HOST_OS)
import GHCJS.DOM.Types (MonadJSM, pFromJSVal)
#else
import GHCJS.DOM.Types (MonadJSM(..))
import Rhyolite.Request.Common (decodeValue')
#endif

import Data.Vessel

-- | This query morphism translates between queries with SelectedCount annotations used in the frontend to do reference counting, and un-annotated queries for use over the wire. This version is for use with the older Functor style of queries and results.
functorToWire
  :: ( Filterable q
     , Functor v
     , QueryResult (q ()) ~ v ()
     , QueryResult (q SelectedCount) ~ v SelectedCount)
  => QueryMorphism (q SelectedCount) (q ())
functorToWire = QueryMorphism
  { -- TODO in principle the <= 0 case should never happen.
    -- Perhaps we should error / impure log / always `Just ()` / etc.,
    -- but we'd need to track down a bunch of instances first.
    _queryMorphism_mapQuery = mapMaybe (\n -> if n <= 0 then Nothing else Just ())
  , _queryMorphism_mapQueryResult = fmap (const (SelectedCount 1))
  }

-- | This query morphism translates between queries with SelectedCount annotations used in the frontend to do reference counting, and un-annotated queries for use over the wire. This version is for use with the newer-style functor-parametric view types (such as Vessel).
vesselToWire
  :: ( View v
     , Monoid (v (Const ()))
     , QueryResult (v (Const ())) ~ v Identity
     , QueryResult (v (Const SelectedCount)) ~ v Identity
     )
  => QueryMorphism (v (Const SelectedCount)) (v (Const ()))
vesselToWire = QueryMorphism
  { _queryMorphism_mapQuery = \q ->
      let deplete (Const n) = if n == mempty then Nothing else Just (Const ())
      in maybe mempty id $ mapMaybeV deplete q
  , _queryMorphism_mapQueryResult = id
  }

type RhyoliteWidgetInternal q r t m = QueryT t q (RequesterT t r Identity m)

newtype RhyoliteWidget q r t m a = RhyoliteWidget { unRhyoliteWidget :: RhyoliteWidgetInternal q r t m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadException)

deriving instance ( Group q
                  , Additive q
                  , Query q
                  , Reflex t
                  , Monad m)
                  => MonadQuery t q (RhyoliteWidget q r t m)

#if !defined(ghcjs_HOST_OS)
instance MonadJSM m => MonadJSM (RhyoliteWidget q r t m) where
  liftJSM' = lift . liftJSM'
#endif

instance MonadTrans (RhyoliteWidget q r t) where
  lift = RhyoliteWidget . lift . lift

instance HasJS x m => HasJS x (RhyoliteWidget q r t m) where
  type JSX (RhyoliteWidget q r t m) = JSX m
  liftJS = lift . liftJS

instance HasDocument m => HasDocument (RhyoliteWidget q r t m) where
  askDocument = RhyoliteWidget . lift . lift $ askDocument

instance (MonadWidget' t m, PrimMonad m) => Requester t (RhyoliteWidget q r t m) where
  type Request (RhyoliteWidget q r t m) = r
  type Response (RhyoliteWidget q r t m) = Identity
  requesting = RhyoliteWidget . requesting
  requesting_ = RhyoliteWidget . requesting_

instance PerformEvent t m => PerformEvent t (RhyoliteWidget q r t m) where
  type Performable (RhyoliteWidget q r t m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance TriggerEvent t m => TriggerEvent t (RhyoliteWidget q r t m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance NotReady t m => NotReady t (RhyoliteWidget q r t m)

instance (DomBuilder t m, MonadHold t m, Ref (Performable m) ~ Ref m, MonadFix m, Group q, Additive q, Eq q, Query q) => DomBuilder t (RhyoliteWidget q r t m) where
  type DomBuilderSpace (RhyoliteWidget q r t m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg (RhyoliteWidget child) = RhyoliteWidget $ element elementTag cfg child
  inputElement = RhyoliteWidget . inputElement
  textAreaElement = RhyoliteWidget . textAreaElement
  selectElement cfg (RhyoliteWidget child) = RhyoliteWidget $ selectElement cfg child
  placeRawElement = RhyoliteWidget . placeRawElement
  wrapRawElement e = RhyoliteWidget . wrapRawElement e

instance (Reflex t, MonadFix m, MonadHold t m, Adjustable t m, Eq q, Group q, Additive q, Query q) => Adjustable t (RhyoliteWidget q r t m) where
  runWithReplace a0 a' = RhyoliteWidget $ runWithReplace (coerce a0) (coerceEvent a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = RhyoliteWidget $ traverseDMapWithKeyWithAdjust (\k v -> unRhyoliteWidget $ f k v) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = RhyoliteWidget $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unRhyoliteWidget $ f k v) (coerce dm0) (coerceEvent dm')
  traverseIntMapWithKeyWithAdjust f dm0 dm' = RhyoliteWidget $ traverseIntMapWithKeyWithAdjust (\k v -> unRhyoliteWidget $ f k v) (coerce dm0) (coerceEvent dm')

instance PostBuild t m => PostBuild t (RhyoliteWidget q r t m) where
  getPostBuild = lift getPostBuild

instance MonadRef m => MonadRef (RhyoliteWidget q r t m) where
  type Ref (RhyoliteWidget q r t m) = Ref m
  newRef = RhyoliteWidget . newRef
  readRef = RhyoliteWidget . readRef
  writeRef r = RhyoliteWidget . writeRef r

instance MonadHold t m => MonadHold t (RhyoliteWidget q r t m) where
  hold a = RhyoliteWidget . hold a
  holdDyn a = RhyoliteWidget . holdDyn a
  holdIncremental a = RhyoliteWidget . holdIncremental a
  buildDynamic a = RhyoliteWidget . buildDynamic a
  headE = RhyoliteWidget . headE

instance MonadSample t m => MonadSample t (RhyoliteWidget q r t m) where
  sample = RhyoliteWidget . sample

instance HasJSContext m => HasJSContext (RhyoliteWidget q r t m) where
  type JSContextPhantom (RhyoliteWidget q r t m) = JSContextPhantom m
  askJSContext = RhyoliteWidget askJSContext

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (RhyoliteWidget q r t m) where
  newEventWithTrigger = RhyoliteWidget . newEventWithTrigger
  newFanEventWithTrigger a = RhyoliteWidget . lift $ newFanEventWithTrigger a

instance (Monad m, Routed t route m) => Routed t route (RhyoliteWidget q r t m) where
  askRoute = lift askRoute

instance (Monad m, SetRoute t route m) => SetRoute t route (RhyoliteWidget q r t m) where
  modifyRoute = lift . modifyRoute

instance (Monad m, RouteToUrl route m) => RouteToUrl route (RhyoliteWidget q r t m) where
  askRouteToUrl = lift askRouteToUrl

deriving instance ( Reflex t
                  , Prerender js t m
                  , MonadFix m
                  , Eq q
                  , Group q
                  , Additive q
                  , Query q
                  ) => Prerender js t (RhyoliteWidget q r t m)

instance PrimMonad m => PrimMonad (RhyoliteWidget q r t m) where
  type PrimState (RhyoliteWidget q r t m) = PrimState m
  primitive = lift . primitive

deriving instance DomRenderHook t m => DomRenderHook t (RhyoliteWidget q r t m)

-- | This synonym adds constraints to MonadRhyoliteWidget that are only available on the frontend, and not via backend rendering.
type MonadRhyoliteFrontendWidget q r t m =
    ( MonadRhyoliteWidget q r t m
    , DomBuilderSpace m ~ GhcjsDomSpace
    , MonadIO m
    , MonadIO (Performable m)
    )

class ( MonadWidget' t m
      , Requester t m
      , R.Request m ~ r
      , Response m ~ Identity
      , Group q
      , Additive q
      , MonadQuery t q m
      ) => MonadRhyoliteWidget q r t m | m -> q r where

instance ( MonadWidget' t m
         , Requester t m
         , R.Request m ~ r
         , Response m ~ Identity
         , Group q
         , Additive q
         , MonadQuery t q m
         ) => MonadRhyoliteWidget q r t m

queryDynUniq :: ( Monad m
                , Reflex t
                , MonadQuery t q m
                , MonadHold t m
                , MonadFix m
                , Eq (QueryResult q)
                )
             => Dynamic t q
             -> m (Dynamic t (QueryResult q))
queryDynUniq = holdUniqDyn <=< queryDyn

watchViewSelector :: ( Monad m
                     , Reflex t
                     , MonadQuery t q m
                     , MonadHold t m
                     , MonadFix m
                     , Eq (QueryResult q)
                     )
                  => Dynamic t q
                  -> m (Dynamic t (QueryResult q))
watchViewSelector = queryDynUniq

--TODO: HasDocument is still not accounted for
type MonadWidget' t m =
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadSample t (Performable m)
  , MonadReflexCreateTrigger t m
  , PostBuild t m
  , PerformEvent t m
  , TriggerEvent t m
  -- , MonadJSM m
  -- , MonadJSM (Performable m)
  -- , HasJSContext m
  -- , HasJSContext (Performable m)
  -- , MonadAsyncException m
  -- , MonadAsyncException (Performable m)
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  )

runObeliskRhyoliteWidget ::
  ( PerformEvent t m
  , TriggerEvent t m
  , PostBuild t m
  , MonadHold t m
  , MonadFix m
  , Prerender x t m
  , HasConfigs m
  , Request req
  , Query qFrontend
  , Group qFrontend
  , Additive qFrontend
  , Eq qWire
  , Monoid (QueryResult qFrontend)
  , FromJSON (QueryResult qWire)
  , ToJSON qWire
  )
  => QueryMorphism qFrontend qWire
  -> Text -- ^ Typically "config/route", config file containing an http/https URL at which the backend will be served.
  -> Encoder Identity Identity (R (FullRoute backendRoute frontendRoute)) PageName -- ^ Checked route encoder
  -> R backendRoute -- ^ The "listen" backend route which is handled by the action produced by 'serveDbOverWebsockets'
  -> RoutedT t (R frontendRoute) (RhyoliteWidget qFrontend req t m) a -- ^ Child widget
  -> RoutedT t (R frontendRoute) m a
runObeliskRhyoliteWidget toWire configRoute enc listenRoute child = do
  obR <- askRoute
  route <- (fmap . fmap) (parseURI . T.unpack . T.strip . T.decodeUtf8) (getConfig configRoute) >>= \case
    Just (Just route) -> pure route
    _ -> error "runObeliskRhyoliteWidget: Unable to parse route config"
  let wsUrl = T.pack (show $ websocketUri route) <> renderBackendRoute enc listenRoute
  lift $ runPrerenderedRhyoliteWidget toWire wsUrl $ runRoutedT child obR

{-# DEPRECATED runPrerenderedRhyoliteWidget "Use runRhyoliteWidget instead" #-}
runPrerenderedRhyoliteWidget
   :: forall qFrontend qWire req m t b x.
      ( PerformEvent t m
      , TriggerEvent t m
      , PostBuild t m
      , MonadHold t m
      , MonadFix m
      , Prerender x t m
      , Request req
      , Query qFrontend
      , Group qFrontend
      , Additive qFrontend
      , Eq qWire
      , Monoid (QueryResult qFrontend)
      , FromJSON (QueryResult qWire)
      , ToJSON qWire
      )
   => QueryMorphism qFrontend qWire
   -> Text
   -> RhyoliteWidget qFrontend req t m b
   -> m b
runPrerenderedRhyoliteWidget toWire url child = snd <$> runRhyoliteWidget toWire url child

runRhyoliteWidget
   :: forall qFrontend qWire req m t b x.
      ( PerformEvent t m
      , TriggerEvent t m
      , PostBuild t m
      , MonadHold t m
      , MonadFix m
      , Prerender x t m
      , Request req
      , Query qFrontend
      , Group qFrontend
      , Additive qFrontend
      , Eq qWire
      , Monoid (QueryResult qFrontend)
      , FromJSON (QueryResult qWire)
      , ToJSON qWire
      )
  => QueryMorphism qFrontend qWire
  -> Text
  -> RhyoliteWidget qFrontend req t m b
  -> m (Dynamic t (AppWebSocket t qWire), b)
runRhyoliteWidget toWire url child = do
  let defAppWebSocket = AppWebSocket
          { _appWebSocket_notification = never
          , _appWebSocket_response = never
          , _appWebSocket_version = never
          , _appWebSocket_connected = constDyn False
          }
  rec (dAppWebSocket :: Dynamic t (AppWebSocket t qWire)) <- prerender (return defAppWebSocket) $ do
          openWebSocket' url request'' nubbedVs
      let (notification :: Event t (QueryResult qWire), response) = (bimap (switch . current) (switch . current) . splitDynPure) $
            ffor dAppWebSocket $ \appWebSocket ->
            ( _appWebSocket_notification appWebSocket
            , _appWebSocket_response appWebSocket
            )
      (request', response') <- matchResponsesWithRequests reqEncoder request $ ffor response $ \(TaggedResponse t v) -> (t, v)
      let request'' = fmap (Map.elems . Map.mapMaybeWithKey (\t v -> case fromJSON v of
            Success (v' :: (Some req)) -> Just $ TaggedRequest t v'
            _ -> Nothing)) request'
      ((a, vs), request) <- flip runRequesterT (fmapMaybe (traverseRequesterData (fmap Identity)) response') $ runQueryT (unRhyoliteWidget child) view
      let (vsDyn :: Dynamic t qFrontend) = incrementalToDynamic (vs :: Incremental t (AdditivePatch qFrontend))
      nubbedVs <- holdUniqDyn (_queryMorphism_mapQuery toWire <$> vsDyn)
      view <- fmap join $ prerender (pure mempty) $ fromNotifications vsDyn $ _queryMorphism_mapQueryResult toWire <$> notification
  return (dAppWebSocket, a)
  where
    reqEncoder :: forall a. req a -> (Aeson.Value, Aeson.Value -> Maybe a)
    reqEncoder r =
      ( whichever @ToJSON @req @a $ Aeson.toJSON r
      , \x -> case has @FromJSON r $ Aeson.fromJSON x of
        Success s-> Just s
        _ -> Nothing
      )

fromNotifications
  :: forall m (t :: *) q. (Query q, MonadHold t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), Reflex t, MonadFix m, Monoid (QueryResult q))
  => Dynamic t q
  -> Event t (QueryResult q)
  -> m (Dynamic t (QueryResult q))
fromNotifications vs ePatch = do
  ePatchThrottled <- throttleBatchWithLag lag ePatch
  foldDyn (\(vs', p) v -> cropView vs' $ p <> v) mempty $ attach (current vs) ePatchThrottled
  where
    lag e = performEventAsync $ ffor e $ \a cb -> liftIO $ cb a

data Decoder f = forall a. FromJSON a => Decoder (f a)

data AppWebSocket t q = AppWebSocket
  { _appWebSocket_notification :: Event t (QueryResult q)
  , _appWebSocket_response :: Event t TaggedResponse
  , _appWebSocket_version :: Event t Text
  , _appWebSocket_connected :: Dynamic t Bool
  }

-- | Open a websocket connection and split resulting incoming traffic into listen notification and api response channels
openWebSocket'
  :: forall r q t x m.
     ( MonadJSM m
     , MonadJSM (Performable m)
     , PostBuild t m
     , TriggerEvent t m
     , PerformEvent t m
     , HasJSContext m
     , HasJS x m
     , MonadFix m
     , MonadHold t m
     , FromJSON (QueryResult q)
     , ToJSON q
     , Request r
     )
  => Text -- ^ A complete URL
  -> Event t [TaggedRequest r] -- ^ Outbound requests
  -> Dynamic t q -- ^ Authenticated listen requests (e.g., ViewSelector updates)
  -> m (AppWebSocket t q)
openWebSocket' url request vs = do
  rec
    let
#if defined(ghcjs_HOST_OS)
      platformDecode = jsonDecode . pFromJSVal
      platformWebSocket cfg = webSocket' url cfg (either (error "webSocket': expected JSVal") return)
#else
      platformDecode = decodeValue' . LBS.fromStrict
      platformWebSocket = webSocket url
#endif
    ws <- platformWebSocket $ def
      & webSocketConfig_send .~ fmap (map (decodeUtf8 . LBS.toStrict . Aeson.encode)) (mconcat
        [ fmap (map WebSocketRequest_Api) request
        , fmap ((:[]) . WebSocketRequest_ViewSelector) $ updated vs :: Event t [WebSocketRequest q r]
        -- NB: It's tempting to try to only send query diffs here, but this must be treated
        -- with care, since the backend needs to know when we cease being interested in things
        -- so that it knows not to send further notifications.
        , tag (fmap ((:[]) . WebSocketRequest_ViewSelector) $ current vs) $ _webSocket_open ws
        ])
  let (eMessages :: Event t (WebSocketResponse q)) = fmapMaybe platformDecode $ _webSocket_recv ws
      notification = fforMaybe eMessages $ \case
        WebSocketResponse_View v -> Just v
        _ -> Nothing
      response = fforMaybe eMessages $ \case
        WebSocketResponse_Api r -> Just r
        _ -> Nothing
      version = fforMaybe eMessages $ \case
        WebSocketResponse_Version v -> Just v
        _ -> Nothing
  connected <- holdDyn False . leftmost $ [True <$ _webSocket_open ws, False <$ _webSocket_close ws]
  return $ AppWebSocket
    { _appWebSocket_notification = notification
    , _appWebSocket_response = response
    , _appWebSocket_version = version
    , _appWebSocket_connected = connected
    }

openWebSocket
  :: forall t x m r q.
     ( MonadJSM m
     , MonadJSM (Performable m)
     , PostBuild t m
     , TriggerEvent t m
     , PerformEvent t m
     , HasJSContext m
     , HasJS x m
     , MonadFix m
     , MonadHold t m
     , Request r
     , FromJSON (QueryResult q)
     , ToJSON q
     )
  => Text -- ^ A complete URL
  -> Event t [TaggedRequest r] -- ^ Outbound requests
  -> Dynamic t q -- ^ current ViewSelector
  -> m ( Event t (QueryResult q)
       , Event t TaggedResponse
       )
openWebSocket murl request vs = do
  aws <- openWebSocket' murl request vs
  return (_appWebSocket_notification aws, _appWebSocket_response aws)

data DeviceToken = DeviceToken_Android Text
                 | DeviceToken_iOS Text
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON DeviceToken
instance ToJSON DeviceToken

data FrontendConfig = FrontendConfig
  { _frontendConfig_registerDeviceForNotifications :: Maybe (DeviceToken -> IO ())
  , _frontendConfig_uriCallback :: Maybe (URI -> IO ())
  , _frontendConfig_warpPort :: Int
  }

instance Default FrontendConfig where
  def = FrontendConfig
    { _frontendConfig_registerDeviceForNotifications = Nothing
    , _frontendConfig_uriCallback = Nothing
    , _frontendConfig_warpPort = 3911
    }

-- | Map application credentials into an "authenticated" subwidget
-- This relieves us of having to carry around application credentials in parts of the application
-- known to be accessible only to authenticated users.
-- For example, you might define a view selector @VS (Map k) a@ and in the authenticated parts of the application
-- the @k@ would be '()'. At top level, this the '()'s would be replaced by a 'QueryMorphism' with the actual credential to
-- be sent to the backend.
mapAuth
  :: forall publicRequest privateRequest q q' a cred t m.
     ( MonadFix m
     , PostBuild t m
     , Query q
     , Group q
     , Additive q
     , Group q'
     , Additive q'
     )
  => cred
  -- ^ The application's authentication token, used to transform api calls made by the authenticated child widget
  -> QueryMorphism q' q
    -- ^ A morphism from a query type supplied by the user, "f", that represents queries made by authenticated widgets, to a the query
    -- type of the application as a whole (which may have authenticated and public components)
  -> QueryT t q' ((RequesterT t (ApiRequest () publicRequest privateRequest)) Identity m) a
  -- ^ The authenticated child widget. It uses '()' as its credential for private requests
  -> RhyoliteWidget q (ApiRequest cred publicRequest privateRequest) t m a
mapAuth token authorizeQuery authenticatedChild = RhyoliteWidget $ do
  v <- askQueryResult
  (a, vs) <- lift $ withRequesterT authorizeReq id $ runQueryT (withQueryT authorizeQuery authenticatedChild) v
  -- tellQueryIncremental vs would seem simpler, but tellQueryDyn is more baked, subtracting off the removals properly.
  tellQueryDyn $ incrementalToDynamic vs
  return a
  where
    authorizeReq
      :: forall x. ApiRequest () publicRequest privateRequest x
      -> ApiRequest cred publicRequest privateRequest x
    authorizeReq = \case
      ApiRequest_Public a -> ApiRequest_Public a
      ApiRequest_Private () a -> ApiRequest_Private token a

