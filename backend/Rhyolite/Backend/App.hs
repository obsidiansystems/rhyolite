{-|
Description:
  A push-pull data pipeline extending from the websocket connection to the database.

The purpose of this module is to provide connected clients (e.g., application
frontends) access to information in the database. Clients access the database
in two ways:

1. via request/response API calls. In FRP terms this can be thought of as
@Event Request -> Event Response@.

2. via "viewselector" queries. These are queries for a "view" of part of the
database that can change over time. In FRP terms, this can be thought of as
@Dynamic Query -> Dynamic QueryResponse@.

Both of these APIs are served over websocket connection between the server and
each connected client. Because users can have overlapping viewselectors, part
of the work of the pipeline is to make it possible to share work when providing
view updates to such clients. This is accomplished by aggregating viewselectors
when possible and then distributing results out to interested clients.

App implementers are probably looking for 'serveDbOverWebsockets' or
'serveVessel': these are the most common entrypoints.

Note that this module doesn't define the actual request/response or
viewselector APIs, and instead focuses on the infrastructure needed to support
such APIs. To actually define a request/response API, see `Rhyolite.Api`. For
viewselector APIs, see `Data.Vessel`.
-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language QuantifiedConstraints #-}
{-# Language RankNTypes #-}
{-# Language RecursiveDo #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
{-# Language TypeApplications #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Rhyolite.Backend.App where

import Control.Category (Category)
import qualified Control.Category as Cat
import Control.Exception (SomeException(..), bracket, try)
import Control.Lens (imapM_)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Align
import Data.Constraint.Extras
import Data.Foldable (fold)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as Map
import Data.MonoidMap (MonoidMap(..), monoidMap)
import Data.Pool (Pool)
import Data.Semigroup ((<>), Semigroup)
import Data.Semigroup.Commutative
import Data.Some (Some(Some))
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Typeable (Typeable)
import Data.Vessel
import Data.Witherable (Filterable(..))
import qualified Database.PostgreSQL.Simple as Pg
import Debug.Trace (trace)
import qualified Network.WebSockets as WS
import Reflex (Group(..))
import Reflex.Query.Base (mapQuery, mapQueryResult)
import Reflex.Query.Class (Query, QueryMorphism(..), QueryResult, SelectedCount(..), crop)
import Snap.Core (Snap)

import Data.Signed (Signed)
import Data.Signed.ClientSession as CS (Key, readSignedWithKey)
import Rhyolite.Api
import Rhyolite.App
import Rhyolite.Concurrent
import Rhyolite.DB.NotifyListen (startNotificationListener)
import Rhyolite.WebSocket (TaggedRequest(..), TaggedResponse(..), WebSocketRequest(..), WebSocketResponse(..))
import Data.Monoid.DecidablyEmpty

-- * Handling a request/response API
-- $api

-- $api
-- Request/Response APIs are defined using a GADT. This function can produce
-- the associated response type for each key of the GADT. Request handlers
-- aren't directly part of the pipeline: delivery of the response is the end of
-- the transaction and there are no automatic subsequent updates. However,
-- changes made to the database via requests may produce notifications that
-- ultimately result in updates being pushed into the pipeline and sent to
-- connected clients.

-- | The request handler is just a function from requests to responses. The
-- forall is necessary here because different requests may produce different
-- response types.
newtype RequestHandler r m = RequestHandler
  { runRequestHandler :: forall a. r a -> m a }

-- * Defining a push/pull data pipeline
-- $pipeline

-- $pipeline
-- This module uses the term "pipeline" to refer to the connection between a
-- consumer and a datasource (e.g., a client application connected via
-- websockets, and the database). Pipelines are different from a
-- request/response api because they allow new information to be "pushed" to
-- the consumer.

-- | A way of connecting a source of data to a consumer of data. The consumer
-- can pull data from the datasource and the datasource can push data to the
-- consumer.
--
-- q is the consumer side
-- q' is the datasource side
newtype Pipeline m q q' = Pipeline
  { unPipeline ::
      QueryHandler q' m ->
        Recipient q m ->
          IO (QueryHandler q m, Recipient q' m)
  }

-- | A way for a pipeline to retrieve data (e.g., use the database to produce a
-- view corresponding to a client's viewselector).
newtype QueryHandler q m = QueryHandler
  { runQueryHandler :: q -> m (QueryResult q) }

-- | A way to push data into a pipeline (e.g., send it to a client)
newtype Recipient q m = Recipient
  { tellRecipient :: QueryResult q -> m () }

-- | A way of attaching to (and later detaching from) a pipeline (e.g., handle
-- client connection and disconnection)
newtype Registrar q = Registrar
  { unRegistrar :: Recipient q IO -> IO (QueryHandler q IO, IO ())
  }

-- | Extends a 'Registrar' with a 'Pipeline'
extendRegistrar :: Pipeline IO q q' -> Registrar q' -> Registrar q
extendRegistrar (Pipeline p) (Registrar r) = Registrar $ \recipient -> do
  rec (qh', close) <- r recipient'
      (qh, recipient') <- p qh' recipient
  return (qh, close)

-- | Prints queries for debugging
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

-- | Prints queries, results, and recipient messages for debugging
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

-- | A key used to track particular consumers. This should not be relevant
-- outside of this module.
newtype ClientKey = ClientKey { unClientKey :: Int }
  deriving (Eq, Ord, Show, Enum)

-- ** Handling multiple clients
-- $multiplex

-- $multiplex
-- The backend needs some way of managing multiple pipelines corresponding to
-- multiple connected clients. It must keep track of each connected client, the
-- queries (viewselectors) issued by that client, and have some way of sending
-- data to that client.

-- | Maintains a Map from connected clients to a 'Recipient' that can be used
-- to transmit data to the clients, and provides an interface for adding and
-- removing clients.
--
-- Takes a function that looks up the 'QueryHandler' for a given client
-- Returns:
--   1. A lookup function into the Map of clients
--   2. A way to register a new client
--      a. A 'QueryHandler' for the newly registered client
--      b. A removal callback to de-register a particular client
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

-- | Produce a multi-client 'Recipient' and a single client 'QueryHandler',
-- given a per-client 'Recipient' lookup function and a 'QueryHandler' for
-- multiple clients.
--
-- 1. Given a recipient lookup function, produces a 'Recipient' that can handle
-- distribution of a map of 'QueryResult's to many recipients
--
-- 2.  Given a multi-client 'QueryHandler', produces a 'QueryHandler' for a
-- single client
fanQuery
  :: forall k q. (Ord k, Monoid (QueryResult q))
  => (k -> IO (Recipient q IO))
    -- ^ Look up a recipient
  -> QueryHandler (MonoidalMap k q) IO
    -- ^ A 'QueryHandler' for multiple clients
  -> ( Recipient (MonoidalMap k q) IO, k -> QueryHandler q IO)
    -- ^ Pair of (1) a function to notify recipients of new 'QueryResult' data and (2) a function to lookup the 'QueryHandler' for a given client
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

-- ** Connecting a datasource

-- | Connect a datasource (e.g., a database) to a pipeline. Data can be
-- "pushed" into the pipeline when there's something new that needs to be sent
-- to connected clients, or it can be pulled, when clients issue a request.
--
-- Data taken from 'getNextNotification' is pushed into the pipeline and when
-- the pipeline pulls data, it is retrieved using 'qh'. In most apps, these
-- "notifications" indicate that some data in the database has changed, and
-- that connected clients may need to be made aware of the change.
feedPipeline
  :: (Group q, Commutative q, PositivePart q, Monoid (QueryResult q))
  => IO (q -> IO (QueryResult q))
  -- ^ Get the next notification to be sent to the pipeline. If no notification
  -- is available, this should block until one is available.
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

-- ** Pipeline implementations
-- $transpose

-- $transpose
-- An important design goal of this module is to ensure that queries are
-- aggregated (so there's some possibility of handling them efficiently and
-- in-bulk) and efficiently fanned out to each interested querier. What this
-- usually means is that we take structures with clients as the keys and
-- queries as the leaves and transpose them so that the queries are the keys
-- and sets of clients are the leaves.

-- | This is typically useful to provide as a last argument to
-- 'serveDbOverWebsockets', as it handles the combinatorics of aggregating the
-- queries of connected clients as provided to the handler for database
-- notifications, and disaggregating the corresponding results of the queries
-- accordingly.
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

-- | This pipeline transposes a structure of vessel-based queries/viewselectors
-- so that the queries are the keys of the structure instead of the values. The
-- point of this transposition is to aggregate common queries. For example,
-- this would transpose a @Map client (vessel (Const SelectedCount))@ so that
-- the clients are "pushed into" the structure: @vessel (Compose (Map client)
-- (Const Selectedcount))@. For each of the aggregated queries at the outer
-- level, we now have a map of clients who are interested in that query.
vesselPipeline :: forall t v m.
  ( QueryResult (t (v (Const SelectedCount))) ~ t (v Identity)
  , QueryResult (v (Compose t (Const SelectedCount))) ~ v (Compose t Identity)
  , Functor m
  , View v
  , Filterable t
  , Foldable t
  , Align t
  , Monoid (v (Const SelectedCount))
  , Monoid (v (Compose t (Const SelectedCount)))
  )
  => Pipeline m (t (v (Const SelectedCount))) (v (Compose t (Const SelectedCount)))
vesselPipeline = vesselGroupPipeline @(Const SelectedCount)

-- | Generalization of 'vesselPipeline' that doesn't necessarily use @Const
-- SelectedCount@.
vesselGroupPipeline
  :: forall g t v m g'.
  ( QueryResult (t (v g)) ~ t (v g')
  , QueryResult (v (Compose t g)) ~ v (Compose t g')
  , Functor m
  , View v
  , Filterable t
  , Foldable t
  , Align t
  , Monoid (v g)
  , Monoid (v (Compose t g))
  )
  => Pipeline m (t (v g)) (v (Compose t g))
vesselGroupPipeline = queryMorphismPipeline transposeView

-- *** Query morphisms

-- | Build a 'Pipeline' out of a 'QueryMorphism'. This is the usual way
-- 'Pipeline's are made.
queryMorphismPipeline :: Functor m => QueryMorphism q q' -> Pipeline m q q'
queryMorphismPipeline qm = Pipeline $ \qh r -> pure $ mapQueryHandlerAndRecipient qm qh r

-- | A query morphism that converts between 'MonoidalMap' and 'MonoidMap'
-- (i.e., between monoidal maps that can have keys with mempty values and a
-- monoidal map where mempty is not admitted as a value).
monoidMapQueryMorphism
  :: (DecidablyEmpty q, Ord k, Monoid q)
  => QueryMorphism (MonoidalMap k q) (MonoidMap k q)
monoidMapQueryMorphism = QueryMorphism
  { _queryMorphism_mapQuery = monoidMap
  , _queryMorphism_mapQueryResult = unMonoidMap
  }

-- | A query morphism from 'MonoidMap' of containers to a container of
-- 'MonoidMap's.
transposeMonoidMap
  :: forall a a' k q qr.
     ( Ord k
     , DecidablyEmpty a
     , Monoid a
     , Semigroup a'
     , Monoid a'
     , DecidablyEmpty (q (MonoidMap k a))
     , DecidablyEmpty (QueryResult (q a))
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

-- | Use a 'QueryMorphism' to transform a 'QueryHandler's query type.
mapQueryHandler :: Functor f => QueryMorphism q q' -> QueryHandler q' f -> QueryHandler q f
mapQueryHandler qm qh = QueryHandler $ \qs -> mapQueryResult qm <$> runQueryHandler qh (mapQuery qm qs)

-- | Use a 'QueryMorphism' to transform a 'Recipient's query type
mapRecipient :: QueryMorphism q q' -> Recipient q m -> Recipient q' m
mapRecipient qm s = Recipient $ \qr -> tellRecipient s $ mapQueryResult qm qr

-- | Map a query morphism over both a 'QueryHandler' and a 'Recipient'. This is
-- useful to ensure that the two match up.
mapQueryHandlerAndRecipient
  :: Functor f
  => QueryMorphism q q'
  -> QueryHandler q' f
  -> Recipient q f
  -> (QueryHandler q f, Recipient q' f)
mapQueryHandlerAndRecipient qm qh s = (mapQueryHandler qm qh, mapRecipient qm s)

-- | Apply a query morphism to a query inside a functor
fmapQueryMorphism ::
  ( Functor f
  , QueryResult (f q) ~ f (QueryResult q)
  , QueryResult (f q') ~ f (QueryResult q') 
  )
  => QueryMorphism q q'
  -> QueryMorphism (f q) (f q')
fmapQueryMorphism qm = QueryMorphism
  { _queryMorphism_mapQuery = fmap $ _queryMorphism_mapQuery qm
  , _queryMorphism_mapQueryResult = fmap $ _queryMorphism_mapQueryResult qm
  }

-- ** Wire format adapters

-- | Reverses 'Rhyolite.Frontend.App.vesselToWire' by re-annotating queries
-- with a 'SelectedCount' used on the backend for group/monoid operations on
-- aggregated queries.
vesselFromWire
  :: ( View v
     , QueryResult (v (Const ())) ~ v Identity
     , QueryResult (v (Const SelectedCount)) ~ v Identity
     )
  => QueryMorphism (v (Const ())) (v (Const SelectedCount))
vesselFromWire = QueryMorphism
  { _queryMorphism_mapQuery = mapV (const (Const 1))
  , _queryMorphism_mapQueryResult = id
  }

-- | This query morphism translates between un-annotated queries for use over
-- the wire, and ones with SelectedCount annotations used in the backend to be
-- able to determine the differences between queries. This version is for use
-- with the older Functor style of queries and results.
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

-- * Query verification
-- $verification

-- $verification
-- Some applications require some authentication before a query response can be
-- delivered. For an implementation that has better error handling, see
-- 'Rhyolite.Vessel.AuthMapV'.

-- | An unauthenticated query
newtype Unverified q = Unverified q

-- | An authenticated query
data Verified cred q = Verified cred q

instance Query q => Query (Verified cred q) where
  type QueryResult (Verified cred q) = QueryResult q
  crop (Verified _ q) qr = crop q qr

instance Query q => Query (Unverified q) where
  type QueryResult (Unverified q) = Maybe (QueryResult q)
  crop (Unverified q) mqr = fmap (crop q) mqr

-- | Checks the signed authentication tokens to validate that a request should
-- get a response.
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
