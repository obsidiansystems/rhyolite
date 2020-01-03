-- | This module contains the implementation of the view/viewselector machinery.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Rhyolite.Backend.App
  ( module Rhyolite.Backend.App
  -- re-export
  , Postgresql
  ) where

import Control.Category (Category)
import qualified Control.Category as Cat
import Control.Exception (SomeException(..), bracket, try)
import Control.Lens (imapM_)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Align
import Data.Constraint.Extras
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as Map
import Data.Foldable (fold)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.MonoidMap (MonoidMap (..), monoidMap)
import Data.Pool (Pool)
import Data.Semigroup (Semigroup, (<>))
import Data.Some (Some(Some))
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Typeable (Typeable)
import Data.Witherable (Filterable(..))
import Debug.Trace (trace)
import Database.Groundhog.Postgresql (Postgresql (..))
import qualified Database.PostgreSQL.Simple as Pg
import Reflex.Query.Base (mapQuery, mapQueryResult)
import Reflex.Query.Class (Query, QueryResult, QueryMorphism (..), SelectedCount (..), crop)
import Snap.Core (MonadSnap, Snap)
import qualified Web.ClientSession as CS
import qualified Network.WebSockets as WS
import Data.Coerce (coerce)
import Data.Vessel
import Reflex (Group(..), Additive)

import Rhyolite.Api
import Rhyolite.App
import Rhyolite.Backend.Listen (startNotificationListener)
import Rhyolite.Concurrent
import Rhyolite.Sign (Signed)
import Rhyolite.Backend.WebSocket (withWebsocketsConnection, getDataMessage, sendEncodedDataMessage)
import Rhyolite.Backend.Sign (readSignedWithKey)
import Rhyolite.WebSocket (TaggedRequest (..), TaggedResponse (..), WebSocketResponse (..), WebSocketRequest (..))

-- | This query morphism translates between un-annotated queries for use over the wire, and ones with SelectedCount annotations used in the backend to be able to determine the differences between queries. This version is for use with the older Functor style of queries and results.
functorFromWire
  :: ( Filterable q
     , Functor v
     , QueryResult (q ()) ~ v ()
     , QueryResult (q SelectedCount) ~ v SelectedCount)
  => QueryMorphism (q ()) (q SelectedCount)
functorFromWire = QueryMorphism
  { _queryMorphism_mapQuery = (1 <$)
  , _queryMorphism_mapQueryResult = void
  }

-- | Handle API requests for a given app
--
-- The request format expected here is 'TaggedRequest'
-- The response format expected here is 'TaggedResponse'
handleAppRequests
  :: (MonadSnap m, Request r)
  => (forall a. r a -> IO a)
  -> m ()
handleAppRequests f = withWebsocketsConnection $ forever . handleAppRequest f

handleAppRequest
  :: (Request r)
  => (forall a. r a -> IO a)
  -> WS.Connection
  -> IO ()
handleAppRequest f conn = do
  (TaggedRequest reqId (Some req)) <- getDataMessage conn
  a <- f req
  sendEncodedDataMessage conn $ TaggedResponse reqId (has @ToJSON req (toJSON a))

-------------------------------------------------------------------------------

-- | Handles API requests
newtype RequestHandler r m = RequestHandler
  { runRequestHandler :: forall a. r a -> m a }

-------------------------------------------------------------------------------

-- | A way for a pipeline to retrieve data
newtype QueryHandler q m = QueryHandler
  { runQueryHandler :: q -> m (QueryResult q) }

-- | A way to push data into a pipeline
newtype Recipient q m = Recipient
  { tellRecipient :: QueryResult q -> m () }

-- | A way of attaching to (and later detaching from) a pipeline
newtype Registrar q = Registrar { unRegistrar :: Recipient q IO -> IO (QueryHandler q IO, IO ()) }

-- | A way of connecting a source of data to a consumer of data. The consumer
-- can pull data from the datasource and the datasource can push data to the
-- consumer.
--
-- q is the consumer side
-- q' is the datasource side
newtype Pipeline m q q' = Pipeline { unPipeline :: QueryHandler q' m -> Recipient q m -> IO (QueryHandler q m, Recipient q' m) }

tracePipelineQuery :: (Show q, Show (QueryResult q)) => String -> Pipeline IO q q
tracePipelineQuery tag = Pipeline $ \qh r -> do
  return 
    ( QueryHandler $ \q -> do
        putStrLn $ tag ++ "(query): " ++ show q
        qr <- runQueryHandler qh q
        return qr
    , Recipient $ \qr -> do
        tellRecipient r qr
    )

tracePipeline :: (Show q, Show (QueryResult q)) => String -> Pipeline IO q q
tracePipeline tag = Pipeline $ \qh r -> do
  putStrLn $ tag ++ "(start)"
  return 
    ( QueryHandler $ \q -> do
        putStrLn $ tag ++ "(query): " ++ show q
        qr <- runQueryHandler qh q
        putStrLn $ tag ++ "(result): " ++ show qr
        return qr
    , Recipient $ \qr -> do
        putStrLn $ tag ++ "(rcpt): " ++ show qr
        tellRecipient r qr
    )

instance Category (Pipeline m) where
  id = Pipeline $ \qh r -> return (qh, r)
  Pipeline f . Pipeline g = Pipeline $ \qh r -> do
    rec (qh'', r') <- g qh' r
        (qh', r'') <- f qh r'
    return (qh'', r'')

-- | A key used to track particular consumers
newtype ClientKey = ClientKey { unClientKey :: Integer }
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------

-- | Produce a multi-client 'Recipient' and a single client 'QueryHandler'
fanQuery
  :: forall k q. (Ord k, Monoid (QueryResult q))
  => (k -> IO (Recipient q IO))
    -- ^ Look up a recipient
  -> QueryHandler (MonoidalMap k q) IO
    -- ^ A 'QueryHandler' for multiple clients
  -> ( Recipient (MonoidalMap k q) IO
       -- Used to notify recipients of new 'QueryResult' data
     , k -> QueryHandler q IO
       -- Used by 'multiplexQuery' to lookup the 'QueryHandler' for a given client
     )
fanQuery lookupRecipient qh = (multiRecipient lookupRecipient, fanQueryHandler qh)
  where
    -- | Given a recipient lookup function, produces a 'Recipient' that can
    -- handle distribution of a map of 'QueryResult's to many recipients
    multiRecipient lookupR = Recipient $ imapM_ $ \k qr -> do
      s <- lookupR k
      tellRecipient s qr
    -- | Given a multi-client 'QueryHandler', produces a 'QueryHandler' for
    -- a single client
    fanQueryHandler qh' = \k -> mapQueryHandler (singletonQuery k) qh'

-- | Maintains a Map from connected clients to a 'Recipient' that can be used to transmit data to the clients
--
-- Takes a function that looks up a the 'QueryHandler' for a given client
-- Returns: 1. A lookup function into the Map of clients
--          2. A way to register a new client
--             a. A 'QueryHandler' for the newly registered client
--             b. A removal callback to de-register a particular client
multiplexQuery
  :: (MonadIO m, Monoid q, Query q, Group q)
  => (ClientKey -> QueryHandler q m)
  -> IO ( ClientKey -> IO (Recipient q m)
        , Recipient q m -> IO (QueryHandler q m, m ())
        )
multiplexQuery lookupQueryHandler = do
  clients <- newIORef (ClientKey 0, Map.empty :: MonoidalMap ClientKey (Recipient q m, q))
  let
    lookupRecipient k = do
      (_, cs) <- readIORef clients
      case Map.lookup k cs of
        Nothing -> do
          putStrLn $ mconcat
            [ "Rhyolite.Backend.App.multiplexQuery: Failed to find sender for key"
            , show k
            , " in known keys "
            , show $ Map.keys cs
            ]
          return $ Recipient $ const $ return ()
        Just s -> return $ fst s

    registerRecipient s = do
      cid <- atomicModifyIORef' clients $ \(cid, recipients) ->
        ((ClientKey (unClientKey cid + 1), Map.insert cid (s, mempty) recipients), cid)
      let
        queryHandler = QueryHandler $ \q -> do
          qOld <- liftIO $ atomicModifyIORef' clients $ \(nextCid, recipients) ->
            ((nextCid, Map.update (\(r, _) -> Just (r, q)) cid recipients), maybe mempty snd $ Map.lookup cid recipients)
          runQueryHandler (lookupQueryHandler cid) (q ~~ qOld)
              
        unregisterRecipient = do
          antiQ <- liftIO $ atomicModifyIORef' clients $ \(nextCid, recipients) ->
            case Map.updateLookupWithKey (\_ _ -> Nothing) cid recipients of
              (Nothing, _) -> trace
                ("Rhyolite.Backend.App.multiplexQuery: Tried to unregister a client key that is not registered " <> show cid)
                ((nextCid, recipients), mempty)
              (Just (_, removedQuery), newRecipients) -> ((nextCid, newRecipients), negateG removedQuery)
          _ <- runQueryHandler (lookupQueryHandler cid) antiQ
          return ()
      return (queryHandler, unregisterRecipient)

  return (lookupRecipient, registerRecipient)

-- | Like 'handleWebsocketConnection' but customized for 'Snap'.
handleWebsocket
  :: forall r q qWire.
     ( Request r
     , Eq (QueryResult q)
     , Monoid (QueryResult q)
     , ToJSON (QueryResult qWire)
     , FromJSON qWire
     , Monoid q
     , Query q
     )
  => Text -- ^ Version
  -> QueryMorphism qWire q -- ^ Query morphism to translate between wire queries and queries with a reasonable group instance. cf. functorFromWire, vesselFromWire
  -> RequestHandler r IO -- ^ Handler for API requests
  -> Registrar q
  -> Snap ()
handleWebsocket v fromWire rh register = withWebsocketsConnection (handleWebsocketConnection v fromWire rh register)

-- | Handles a websocket connection given a raw connection.
handleWebsocketConnection
  :: forall r q qWire.
    ( Request r
    , Eq (QueryResult q)
    , ToJSON (QueryResult qWire)
    , FromJSON qWire
    , Monoid q
    , Query q
    )
  => Text -- ^ Version
  -> QueryMorphism qWire q -- ^ Query morphism to translate between wire queries and queries with a reasonable group instance. cf. functorFromWire, vesselFromWire
  -> RequestHandler r IO -- ^ Handler for API requests
  -> Registrar q
  -> WS.Connection
  -> IO ()
handleWebsocketConnection v fromWire rh register conn = do
  let sender = Recipient $ sendEncodedDataMessage conn . (\a -> WebSocketResponse_View (_queryMorphism_mapQueryResult fromWire a) :: WebSocketResponse qWire)
  sendEncodedDataMessage conn (WebSocketResponse_Version v :: WebSocketResponse qWire)
  bracket (unRegistrar register sender) snd $ \(vsHandler, _) -> forever $ do
    (wsr :: WebSocketRequest qWire r) <- liftIO $ getDataMessage conn
    case wsr of
      WebSocketRequest_Api (TaggedRequest reqId (Some req)) -> do
        a <- runRequestHandler rh req
        sendEncodedDataMessage conn
          (WebSocketResponse_Api $ TaggedResponse reqId (has @ToJSON req (toJSON a)) :: WebSocketResponse qWire)
      WebSocketRequest_ViewSelector new -> do
        qr <- runQueryHandler vsHandler (_queryMorphism_mapQuery fromWire new)
        when (qr /= mempty) $ do
          sendEncodedDataMessage conn (WebSocketResponse_View (_queryMorphism_mapQueryResult fromWire qr) :: WebSocketResponse qWire)

-------------------------------------------------------------------------------

-- | Connect a datasource (e.g., a database) to a pipeline
--
-- Data taken from 'getNextNotification' is pushed into the pipeline and
-- when the pipeline pulls data, it is retrieved using 'qh'
feedPipeline
  :: (Group q, Additive q, PositivePart q, Monoid (QueryResult q))
  => IO (q -> IO (QueryResult q))
  -- ^ Get the next notification to be sent to the pipeline. If no notification
  -- is available, this should block until one is available
  -> QueryHandler q IO
  -- ^ Retrieve data when requested by pipeline
  -> Recipient q IO
  -- ^ A way to push data into the pipeline
  -> IO (QueryHandler q IO, IO ())
  -- ^ A way for the pipeline to request data
feedPipeline getNextNotification qh r = do
  currentQuery <- newIORef mempty
  let qhSaveQuery = QueryHandler $ \new -> do
        atomicModifyIORef' currentQuery $ \old -> (new <> old, ())
        case positivePart new of
          Nothing -> return mempty
          Just q -> runQueryHandler qh q
  stopWorker <- worker 10000 $ do
    nm <- getNextNotification
    q <- readIORef currentQuery
    qr <- nm q
    tellRecipient r qr
  return (qhSaveQuery, stopWorker)

-- | Connects the pipeline to websockets consumers
connectPipelineToWebsockets
  :: ( Request r
     , Monoid q
     , Monoid (QueryResult q)
     , Eq (QueryResult q)
     , FromJSON qWire
     , ToJSON (QueryResult qWire)
     , Query q
     , Group q
     )
  => Text -- ^ Version
  -> QueryMorphism qWire q -- ^ Query morphism to translate between wire queries and queries with a reasonable group instance. cf. functorFromWire, vesselFromWire
  -> RequestHandler r IO
  -- ^ API handler
  -> QueryHandler (MonoidalMap ClientKey q) IO
  -- ^ A way to retrieve more data for each consumer
  -> IO (Recipient (MonoidalMap ClientKey q) IO, Snap ())
  -- ^ A way to send data to many consumers and a handler for websockets connections
connectPipelineToWebsockets = connectPipelineToWebsocketsRaw withWebsocketsConnection

connectPipelineToWebsocketsRaw
  :: ( Request r
     , Monoid q
     , Monoid (QueryResult q)
     , Eq (QueryResult q)
     , FromJSON qWire
     , ToJSON (QueryResult qWire)
     , Query q
     , Group q
     )
  => ((WS.Connection -> IO ()) -> m a) -- ^ Websocket handler
  -> Text -- ^ Version
  -> QueryMorphism qWire q -- ^ Query morphism to translate between wire queries and queries with a reasonable group instance. cf. functorFromWire, vesselFromWire
  -> RequestHandler r IO
  -- ^ API handler
  -> QueryHandler (MonoidalMap ClientKey q) IO
  -- ^ A way to retrieve more data for each consumer
  -> IO (Recipient (MonoidalMap ClientKey q) IO, m a)
  -- ^ A way to send data to many consumers and a handler for websockets connections
connectPipelineToWebsocketsRaw withWsConn ver fromWire rh qh = do
  (allRecipients, registerRecipient) <- connectPipelineToWebsockets' qh
  return (allRecipients, withWsConn (handleWebsocketConnection ver fromWire rh registerRecipient))

-- | Like 'connectPipelineToWebsockets' but returns a Registrar that can
-- be used to construct a handler for a particular client
connectPipelineToWebsockets'
  :: ( Monoid q
     , Monoid (QueryResult q)
     , Query q
     , Group q
     )
  => QueryHandler (MonoidalMap ClientKey q) IO
  -> IO (Recipient (MonoidalMap ClientKey q) IO, Registrar q)
  -- ^ A way to send data to many consumers, and a way to register new consumers
connectPipelineToWebsockets' qh = do
  rec (lookupRecipient, registerRecipient) <- multiplexQuery clientQueryHandler
      let (allRecipients, clientQueryHandler) = fanQuery lookupRecipient qh
  return (allRecipients, Registrar registerRecipient)

-- | Extends a 'Registrar' with a 'Pipeline'
extendRegistrar :: Pipeline IO q q' -> Registrar q' -> Registrar q
extendRegistrar (Pipeline p) (Registrar r) = Registrar $ \recipient -> do
  rec (qh', close) <- r recipient'
      (qh, recipient') <- p qh' recipient
  return (qh, close)

-------------------------------------------------------------------------------

serveDbOverWebsockets
  :: ( Request r
     , Monoid q'
     , Semigroup q'
     , Eq q
     , Monoid q
     , FromJSON qWire
     , ToJSON (QueryResult qWire)
     , Query q
     , Group q
     , Monoid (QueryResult q)
     , Eq (QueryResult q)
     , FromJSON notifyMessage
     , Query q'
     , Group q'
     , Additive q'
     , PositivePart q'
     )
  => Pool Postgresql
  -> RequestHandler r IO
  -> (notifyMessage -> q' -> IO (QueryResult q'))
  -> QueryHandler q' IO
  -> QueryMorphism qWire q
  -> Pipeline IO (MonoidalMap ClientKey q) q'
  -> IO (Snap (), IO ())
serveDbOverWebsockets pool rh nh qh fromWire pipeline = do
  mver <- try (T.readFile "version")
  let version = either (\(SomeException _) -> "") id mver
  serveDbOverWebsocketsRaw withWebsocketsConnection version fromWire pool rh nh qh pipeline

serveDbOverWebsocketsRaw
  :: forall notifyMessage qWire q q' r m a.
     ( Request r
     , FromJSON qWire
     , ToJSON (QueryResult qWire)
     , Monoid q'
     , Semigroup q'
     , Eq q
     , Monoid q
     , Query q
     , Group q
     , Monoid (QueryResult q)
     , Eq (QueryResult q)
     , FromJSON notifyMessage
     , Query q'
     , Group q'
     , Additive q'
     , PositivePart q'
     )
  => ((WS.Connection -> IO ()) -> m a)
  -> Text -- ^ version
  -> QueryMorphism qWire q -- ^ Query morphism to translate between wire queries and queries with a reasonable group instance. cf. functorFromWire, vesselFromWire
  -> Pool Postgresql
  -> RequestHandler r IO
  -> (notifyMessage -> q' -> IO (QueryResult q'))
  -> QueryHandler q' IO
  -> Pipeline IO (MonoidalMap ClientKey q) q'
  -> IO (m a, IO ())
serveDbOverWebsocketsRaw withWsConn version fromWire db handleApi handleNotify handleQuery pipe = do
  (getNextNotification, finalizeListener) <- startNotificationListener db
  rec (qh, finalizeFeed) <- feedPipeline (handleNotify <$> getNextNotification) handleQuery r
      (qh', r) <- unPipeline pipe qh r'
      (r', handleListen) <- connectPipelineToWebsocketsRaw withWsConn version fromWire handleApi qh'
  return (handleListen, finalizeFeed >> finalizeListener)

convertPostgresPool :: Pool Pg.Connection -> Pool Postgresql
convertPostgresPool = coerce

-- | This is typically useful to provide as a last argument to serveDbOverWebsockets, as it handles
-- the combinatorics of aggregating the queries of connected clients as provided to the handler for
-- database notifications, and disaggregating the corresponding results of the queries accordingly.
standardPipeline
  :: forall m k q qr.
    ( QueryResult (q (MonoidMap k SelectedCount)) ~ qr (MonoidMap k SelectedCount)
    , QueryResult (q SelectedCount) ~ qr SelectedCount
    , Functor m
    , Ord k
    , Align q
    , Foldable qr
    , Filterable qr
    )
  => Pipeline m (MonoidalMap k (q SelectedCount)) (q (MonoidMap k SelectedCount))
standardPipeline = queryMorphismPipeline
  (QueryMorphism (fmap MonoidMap . condense)
                 (disperse . fmap unMonoidMap))

-- | This is also useful as a final argument to serveDbOverWebsockets, in the case that you're using Vessel-style queries/views.
vesselPipeline
  :: forall m t v.
    ( QueryResult (t (v (Const ()))) ~ t (v Identity)
    , QueryResult (v (Compose t (Const ()))) ~ v (Compose t Identity)
    , Monoid (v (Compose t (Const ())))
    , Monoid (v (Const ()))
    , Functor m
    , View v
    , Foldable t
    , Filterable t
    , Align t
    )
  => Pipeline m (t (v (Const ()))) (v (Compose t (Const ())))
vesselPipeline = queryMorphismPipeline transposeView

-------------------------------------------------------------------------------

monoidMapQueryMorphism :: (Eq q, Ord k, Monoid q) => QueryMorphism (MonoidalMap k q) (MonoidMap k q)
monoidMapQueryMorphism = QueryMorphism
  { _queryMorphism_mapQuery = monoidMap
  , _queryMorphism_mapQueryResult = unMonoidMap
  }

transposeMonoidMap
  :: forall a a' k q qr.
     ( Ord k
     , Eq a
     , Monoid a
     , Semigroup a'
     , Monoid a'
     , Eq (q (MonoidMap k a))
     , Eq (QueryResult (q a))
     , Foldable qr
     , Functor q
     , Functor qr
     , Filterable qr
     , Monoid (q (MonoidMap k a))
     , Monoid (QueryResult (q a))
     , QueryResult (q (MonoidMap k a)) ~ qr (MonoidMap k a')
     , QueryResult (q a) ~ qr a'
     )
  => QueryMorphism (MonoidMap k (q a)) (q (MonoidMap k a))
transposeMonoidMap = QueryMorphism
  { _queryMorphism_mapQuery = aggregateQueries
  , _queryMorphism_mapQueryResult = distributeResults
  }
  where
    aggregateQueries :: MonoidMap k (q a) -> q (MonoidMap k a)
    aggregateQueries = fold . monoidMap . Map.mapWithKey (\k q -> fmap (monoidMap . Map.singleton k) q) . unMonoidMap
    distributeResults :: qr (MonoidMap k a') -> MonoidMap k (qr a')
    distributeResults v = monoidMap $ Map.mapWithKey (\k _ -> mapMaybe (Map.lookup k . unMonoidMap) v) $ fold $ fmap unMonoidMap v

mapQueryHandlerAndRecipient
  :: Functor f
  => QueryMorphism q q'
  -> QueryHandler q' f
  -> Recipient q f
  -> (QueryHandler q f, Recipient q' f)
mapQueryHandlerAndRecipient qm qh s = (mapQueryHandler qm qh, mapRecipient qm s)

mapQueryHandler :: Functor f => QueryMorphism q q' -> QueryHandler q' f -> QueryHandler q f
mapQueryHandler qm qh = QueryHandler $ \qs -> mapQueryResult qm <$> runQueryHandler qh (mapQuery qm qs)

mapRecipient :: QueryMorphism q q' -> Recipient q m -> Recipient q' m
mapRecipient qm s = Recipient $ \qr -> tellRecipient s $ mapQueryResult qm qr

fmapQueryMorphism
  :: ( Functor f, QueryResult (f q) ~ f (QueryResult q)
     , QueryResult (f q') ~ f (QueryResult q') )
  => QueryMorphism q q'
  -> QueryMorphism (f q) (f q')
fmapQueryMorphism qm = QueryMorphism
  { _queryMorphism_mapQuery = fmap $ _queryMorphism_mapQuery qm
  , _queryMorphism_mapQueryResult = fmap $ _queryMorphism_mapQueryResult qm
  }

queryMorphismPipeline :: Functor m => QueryMorphism q q' -> Pipeline m q q'
queryMorphismPipeline qm = Pipeline $ \qh r -> pure $ mapQueryHandlerAndRecipient qm qh r

-------------------------------------------------------------------------------

newtype Unverified q = Unverified q
data Verified cred q = Verified cred q

instance Query q => Query (Verified cred q) where
  type QueryResult (Verified cred q) = QueryResult q
  crop (Verified _ q) qr = crop q qr

instance Query q => Query (Unverified q) where
  type QueryResult (Unverified q) = Maybe (QueryResult q)
  crop (Unverified q) mqr = fmap (crop q) mqr

verifyQuery
  :: (Monoid (QueryResult q), Typeable cred, FromJSON cred)
  => CS.Key
  -> Recipient (MonoidalMap (Signed cred) (Unverified q)) IO
  -> QueryHandler (MonoidalMap (Signed cred) (Verified cred q)) IO
  -- ^ A way to look up more information
  -> ( Recipient (MonoidalMap (Signed cred) (Verified cred q)) IO
      -- Used by the outside world to tell us there is more data
     , QueryHandler (MonoidalMap (Signed cred) (Unverified q)) IO
     -- Used by individual clients to indicate that they want more data
     )
verifyQuery csk sender vsHandler =
  let runVerifiedRecipient = Recipient $ \qrs -> tellRecipient sender $ Map.mapWithKey (\token qr -> qr <$ readSignedWithKey csk token) qrs
      runUnverifiedVSHandler = QueryHandler $ \qs -> do
        v <- runQueryHandler vsHandler $
          Map.mapMaybeWithKey (\token (Unverified q) ->
            case readSignedWithKey csk token of
              Just t -> Just $ Verified t q
              Nothing -> Nothing) qs
        return $ Map.mapWithKey (\k r -> r <$ readSignedWithKey csk k) v -- TODO eliminate this redundant checking
  in (runVerifiedRecipient, runUnverifiedVSHandler)
