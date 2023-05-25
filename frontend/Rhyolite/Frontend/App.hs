{-| Description: Connect clients to a rhyolite backend

Frontend part of the view/viewselector implementation. Here we have the
definition of 'RhyoliteWidget' and the functions to run it, 'runRhyoliteWidget'
and 'runObeliskRhyoliteWidget'. We also have the 'watchViewSelector' function
that's used in the frontend module of a typical app.
-}

{-# Language CPP #-}
{-# Language ConstraintKinds #-}
{-# Language DeriveGeneric #-}
{-# Language ExistentialQuantification #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language InstanceSigs #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language PolyKinds #-}
{-# Language RankNTypes #-}
{-# Language RecursiveDo #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}

module Rhyolite.Frontend.App where

import Control.Applicative
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
import Data.Text.Encoding (decodeUtf8)
import Data.Witherable (Filterable)
import GHC.Generics (Generic)
import Network.URI (URI, uriPath)
import Obelisk.Frontend.Cookie
import Obelisk.Route.Frontend (RouteToUrl(..), Routed(..), SetRoute(..))
import qualified Reflex as R
import Reflex.Dom.Core hiding (MonadWidget, Request)
import Reflex.Host.Class
import Reflex.Time (throttleBatchWithLag)

import Rhyolite.Api
import Rhyolite.WebSocket

import Obelisk.Configs
import Obelisk.Route
import Obelisk.Route.Frontend

#if defined(ghcjs_HOST_OS)
import GHCJS.DOM.Types (MonadJSM, pFromJSVal)
#else
import GHCJS.DOM.Types (MonadJSM(..))
#endif

import Data.Vessel
import Data.Vessel.ViewMorphism

-- * Viewselectors / Queries

-- ** Convert to wire format

-- | This query morphism translates between queries with SelectedCount
-- annotations used in the frontend to do reference counting, and un-annotated
-- queries for use over the wire. This version is for use with the older
-- Functor style of queries and results.
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

-- | This query morphism translates between queries with SelectedCount
-- annotations used in the frontend to do reference counting, and un-annotated
-- queries for use over the wire. This version is for use with the newer-style
-- functor-parametric view types (such as Vessel).
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

-- * Widgets that can make requests/queries

-- | Encapsulates the widget's ability to make requests and issue queries
type RhyoliteWidgetInternal q r t m = QueryT t q (RequesterT t r Identity m)

-- | A widget that can make requests and issue view selectors
newtype RhyoliteWidget q r t m a = RhyoliteWidget { unRhyoliteWidget :: RhyoliteWidgetInternal q r t m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadException, HasCookies)

deriving instance
  ( Group q
  , Additive q
  , Query q
  , Reflex t
  , Monad m
  ) => MonadQuery t q (RhyoliteWidget q r t m)

#if !defined(ghcjs_HOST_OS)
instance MonadJSM m => MonadJSM (RhyoliteWidget q r t m) where
  liftJSM' = lift . liftJSM'
#endif

instance MonadTrans (RhyoliteWidget q r t) where
  lift = RhyoliteWidget . lift . lift

instance HasDocument m => HasDocument (RhyoliteWidget q r t m) where
  askDocument = RhyoliteWidget . lift . lift $ askDocument

instance HasConfigs m => HasConfigs (RhyoliteWidget q r t m)

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

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (RhyoliteWidget q r t m) where
  newEventWithTrigger = RhyoliteWidget . newEventWithTrigger
  newFanEventWithTrigger a = RhyoliteWidget . lift $ newFanEventWithTrigger a

instance (Monad m, Routed t route m) => Routed t route (RhyoliteWidget q r t m) where
  askRoute = lift askRoute

instance (Monad m, SetRoute t route m) => SetRoute t route (RhyoliteWidget q r t m) where
  modifyRoute = lift . modifyRoute

instance (Monad m, RouteToUrl route m) => RouteToUrl route (RhyoliteWidget q r t m) where
  askRouteToUrl = lift askRouteToUrl

deriving instance
  ( Reflex t
    , Prerender t m
    , MonadFix m
    , Eq q
    , Group q
    , Additive q
    , Query q
  ) => Prerender t (RhyoliteWidget q r t m)

instance PrimMonad m => PrimMonad (RhyoliteWidget q r t m) where
  type PrimState (RhyoliteWidget q r t m) = PrimState m
  primitive = lift . primitive

deriving instance DomRenderHook t m => DomRenderHook t (RhyoliteWidget q r t m)

-- | This synonym adds constraints to MonadRhyoliteWidget that are only
-- available on the frontend, and not via backend rendering.
type MonadRhyoliteFrontendWidget q r t m =
    ( MonadRhyoliteWidget q r t m
    , DomBuilderSpace m ~ GhcjsDomSpace
    , MonadIO m
    , MonadIO (Performable m)
    )

-- | Just a collection of constraints
class
  ( MonadWidget' t m
  , Requester t m
  , R.Request m ~ r
  , Response m ~ Identity
  , Group q
  , Additive q
  , MonadQuery t q m
  ) => MonadRhyoliteWidget q r t m | m -> q r where

instance
  ( MonadWidget' t m
  , Requester t m
  , R.Request m ~ r
  , Response m ~ Identity
  , Group q
  , Additive q
  , MonadQuery t q m
  ) => MonadRhyoliteWidget q r t m

-- | A collection of common widget constraints
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

-- ** Run a rhyolite frontend

-- | Runs a rhyolite frontend widget that uses obelisk routing. See 'runRhyoliteWidget'.
runObeliskRhyoliteWidget ::
  ( PerformEvent t m
  , TriggerEvent t m
  , PostBuild t m
  , MonadHold t m
  , MonadFix m
  , Prerender t m
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
  -- ^ Wire format morphism
  -> URI -- ^ http/https URL at which the backend will be served.
  -> Encoder Identity Identity (R (FullRoute backendRoute frontendRoute)) PageName -- ^ Checked route encoder
  -> R backendRoute -- ^ The "listen" backend route which is handled by the action produced by 'Rhyolite.Backend.App.serveDbOverWebsockets'
  -> RoutedT t route (RhyoliteWidget qFrontend req t m) a -- ^ Child widget
  -> RoutedT t route m (Dynamic t (AppWebSocket t qWire), a)
runObeliskRhyoliteWidget toWire route enc listenRoute child = do
  let wsUrl = T.pack $ show $ (websocketUri route) { uriPath = T.unpack $ T.takeWhile (/= '?') $ renderBackendRoute enc listenRoute }
  mapRoutedT (runRhyoliteWidget toWire wsUrl) child

-- | Runs a rhyolite frontend widget that opens a websocket connection and can
-- issue requests and queries over that connection.
runRhyoliteWidget
   :: forall qFrontend qWire req m t b.
      ( PerformEvent t m
      , TriggerEvent t m
      , PostBuild t m
      , MonadHold t m
      , MonadFix m
      , Prerender t m
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
  -- ^ Wire format morphism for queries
  -> Text
  -- ^ Websocket url
  -> RhyoliteWidget qFrontend req t m b
  -- ^ The widget to run. This widget can make requests/queries
  -> m (Dynamic t (AppWebSocket t qWire), b)
runRhyoliteWidget toWire url child = do
  let defAppWebSocket = AppWebSocket
          { _appWebSocket_notification = never
          , _appWebSocket_response = never
          , _appWebSocket_version = never
          , _appWebSocket_connected = constDyn False
          }
  rec (dAppWebSocket :: Dynamic t (AppWebSocket t qWire)) <- prerender (return defAppWebSocket) $ do
          openWebSocket url request'' nubbedVs
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

-- | Receive the results of a 'Query' as a stream of 'Event's and combine them
-- into a 'Dynamic' 'QueryResult'
fromNotifications :: forall m (t :: *) q.
  ( Query q
  , MonadHold t m
  , PerformEvent t m
  , TriggerEvent t m
  , MonadIO (Performable m)
  , Reflex t
  , MonadFix m
  , Monoid (QueryResult q)
  )
  => Dynamic t q
  -> Event t (QueryResult q)
  -> m (Dynamic t (QueryResult q))
fromNotifications vs ePatch = do
  ePatchThrottled <- throttleBatchWithLag lag ePatch
  foldDyn (\(vs', p) v -> crop vs' $ p <> v) mempty $ attach (current vs) ePatchThrottled
  where
    lag e = performEventAsync $ ffor e $ \a cb -> liftIO $ cb a

-- | Information pertaining to a websocket connection, including received
-- messages and connection state
data AppWebSocket t q = AppWebSocket
  { _appWebSocket_notification :: Event t (QueryResult q)
  , _appWebSocket_response :: Event t TaggedResponse
  , _appWebSocket_version :: Event t Text
  , _appWebSocket_connected :: Dynamic t Bool
  }

-- | Open a websocket connection and split resulting incoming traffic into
-- listen notification and api response channels
openWebSocket
  :: forall r q t m.
     ( MonadJSM m
     , MonadJSM (Performable m)
     , PostBuild t m
     , TriggerEvent t m
     , PerformEvent t m
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
openWebSocket url request vs = do
  rec
    let
#if defined(ghcjs_HOST_OS)
      platformDecode = jsonDecode . pFromJSVal
      platformWebSocket cfg = webSocket' url cfg (either (error "webSocket': expected JSVal") return)
#else
      platformDecode = decodeStrict'
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

-- ** Issue queries

-- | Issue a query and produce a dynamic result. Note that the dynamic will
-- only update when the value changes to a new value (updates to the same value
-- will not fire an 'updated' event)
queryDynUniq ::
  ( Monad m
  , Reflex t
  , MonadQuery t q m
  , MonadHold t m
  , MonadFix m
  , Eq (QueryResult q)
  )
  => Dynamic t q
  -> m (Dynamic t (QueryResult q))
queryDynUniq = holdUniqDyn <=< queryDyn

-- | Synonym for 'queryDynUniq`
watchViewSelector ::
  ( Monad m
  , Reflex t
  , MonadQuery t q m
  , MonadHold t m
  , MonadFix m
  , Eq (QueryResult q)
  )
  => Dynamic t q
  -> m (Dynamic t (QueryResult q))
watchViewSelector = queryDynUniq

-- | watch a viewselector defined with a ViewMorphism
watchView
  :: forall q partial (a :: *) m t.
  ( QueryResult q ~ ViewQueryResult q
  , MonadQuery t q m
  , Reflex t
  , MonadHold t m
  , Alternative partial
  )
  => Dynamic t (ViewMorphism Identity partial (Const SelectedCount a) q)
  -> m (Dynamic t (partial a))
watchView q = (fmap.fmap) runIdentity <$> queryViewMorphism 1 q
-- Reminder to self, this ^^^^^^^^^^^ identity is the QueryResult for the Const g
-- in the ViewMorphism and is unrelated to the fixed Identity in the fourth
-- parameter.

-- | The type of 'Path' required to do a 'watch'.
type FullPath a v b = Path (Const SelectedCount a) (v (Const SelectedCount)) (v Identity) (Identity b)

-- | Use a dynamic 'Path' to construct and query a view selector,
-- and to process the resulting view.
watch :: forall t v a b m.
  ( Reflex t
  , MonadQuery t (v (Const SelectedCount)) m
  , QueryResult (v (Const SelectedCount)) ~ v Identity
  , MonadHold t m
  , MonadFix m
  , Eq (v Identity)
  )
  => Dynamic t (FullPath a v b)
  -> m (Dynamic t (Maybe b))
watch pathDyn = do
  r <- watchViewSelector . ffor pathDyn $ \path -> _path_to path (Const 1)
  return $ fmap (fmap runIdentity) . _path_from <$> pathDyn <*> r


-- * iOS and Android capabilities

-- | Device identifiers
data DeviceToken = DeviceToken_Android Text
                 | DeviceToken_iOS Text
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON DeviceToken
instance ToJSON DeviceToken

-- | Configuration for mobile-specific capabilities
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

-- * Credential management

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
