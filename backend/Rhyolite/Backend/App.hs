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
{-# LANGUAGE UndecidableInstances #-}

module Rhyolite.Backend.App where

import Control.Category (Category)
import qualified Control.Category as Cat
import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Lens (imapM_)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (evalStateT, get, put)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON, toJSON)
import Data.Map.Monoidal (MonoidalMap)
import qualified Rhyolite.Map.Monoidal as Map
import Data.Foldable (fold)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.MonoidMap (MonoidMap (..), monoidMap)
import Data.Pool (Pool)
import Data.Semigroup (Semigroup, (<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Debug.Trace (trace)
import Database.Groundhog.Postgresql (Postgresql)
import Reflex (FunctorMaybe (..))
import Reflex.Patch (Group, negateG, (~~))
import Reflex.Query.Base (mapQuery, mapQueryResult)
import Reflex.Query.Class (Query, QueryResult, QueryMorphism (..), SelectedCount (..), crop)
import Snap.Core (MonadSnap, Snap)
import qualified Web.ClientSession as CS

import Rhyolite.Api (AppRequest)
import Rhyolite.App (HasRequest, HasView, ViewSelector, singletonQuery)
import Rhyolite.Backend.Listen (NotifyMessage, startNotificationListener)
import Rhyolite.Sign (Signed)
import Rhyolite.Backend.WebSocket (withWebsocketsConnection, getDataMessage, sendEncodedDataMessage)
import Rhyolite.Request.Class (SomeRequest (..))
import Rhyolite.Backend.Sign (readSignedWithKey)
import Rhyolite.WebSocket

-- | Handle API requests for a given app
--
-- The request format expected here is 'TaggedRequest'
-- The response format expected here is 'TaggedResponse'
handleAppRequests
  :: (MonadSnap m, HasRequest app)
  => (forall a. AppRequest app a -> IO a)
  -> m ()
handleAppRequests f = withWebsocketsConnection $ \conn -> forever $ do
  (TaggedRequest reqId (SomeRequest req)) <- getDataMessage conn
  a <- f req
  sendEncodedDataMessage conn $ TaggedResponse reqId (toJSON a)

-------------------------------------------------------------------------------

-- | Handles API requests
newtype RequestHandler app m = RequestHandler
  { runRequestHandler :: forall a. AppRequest app a -> m a }

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
  :: (MonadIO m, Group q)
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
          liftIO $ atomicModifyIORef' clients $ \(nextCid, recipients) ->
            ((nextCid, Map.update (\(r, oldQ) -> Just (r, oldQ <> q)) cid recipients), ())
          runQueryHandler (lookupQueryHandler cid) q

        unregisterRecipient = do
          antiQ <- liftIO $ atomicModifyIORef' clients $ \(nextCid, recipients) ->
            case Map.updateLookupWithKey (\_ _ -> Nothing) cid recipients of
              (Nothing, _) -> trace
                ("Rhyolite.Backend.App.multiplexQuery: Tried to unregister a client key that is not registered " <> show cid)
                ((nextCid, recipients), mempty)
              (Just (_, removedQuery), newRecipients) -> ((nextCid, newRecipients), negateG removedQuery)

          -- TODO: Should we have a way of ensuring that this doesn't actually cause a query to be run?
          -- It shouldn't cause the query to be run again but it depends on if the callee will notice
          -- that the new query is strictly smaller than the old one.
          runQueryHandler (lookupQueryHandler cid) antiQ
          return ()

      return (queryHandler, unregisterRecipient)

  return (lookupRecipient, registerRecipient)

-- | Handles a websocket connection
handleWebsocket
  :: forall app.
     ( HasView app
     , HasRequest app
     , Eq (ViewSelector app SelectedCount) )
  => Text -- ^ Version
  -> RequestHandler app IO -- ^ Handler for API requests
  -> Registrar (ViewSelector app SelectedCount)
  -> Snap ()
handleWebsocket v rh register = withWebsocketsConnection $ \conn -> do
  let sender = Recipient $ sendEncodedDataMessage conn . (\a -> WebSocketResponse_View (void a) :: WebSocketResponse app)
  sendEncodedDataMessage conn (WebSocketResponse_Version v :: WebSocketResponse app)
  bracket (unRegistrar register sender) snd $ \(vsHandler, _) -> flip evalStateT mempty $ forever $ do
    (wsr :: WebSocketRequest app (AppRequest app)) <- liftIO $ getDataMessage conn
    case wsr of
      WebSocketRequest_Api (TaggedRequest reqId (SomeRequest req)) -> lift $ do
        a <- runRequestHandler rh req
        sendEncodedDataMessage conn
          (WebSocketResponse_Api $ TaggedResponse reqId (toJSON a) :: WebSocketResponse app)
      WebSocketRequest_ViewSelector new -> do
        old <- get
        let new' = SelectedCount 1 <$ new
            vsDiff = new' ~~ old
        when (vsDiff /= mempty) $ do
          qr <- lift $ runQueryHandler vsHandler vsDiff
          put new'
          when (qr /= mempty) $ lift $
            sendEncodedDataMessage conn (WebSocketResponse_View (void qr) :: WebSocketResponse app)

-------------------------------------------------------------------------------

-- | Connect a datasource (e.g., a database) to a pipeline
--
-- Data taken from 'getNextNotification' is pushed into the pipeline and
-- when the pipeline pulls data, it is retrieved using 'qh'
feedPipeline
  :: (Monoid q, Semigroup q)
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
  let qhSaveQuery = QueryHandler $ \q -> do
        atomicModifyIORef' currentQuery $ \old -> (q <> old, ())
        runQueryHandler qh q
  tid <- forkIO . forever $ do
    nm <- getNextNotification
    q <- readIORef currentQuery
    qr <- nm q
    tellRecipient r qr
  return (qhSaveQuery, killThread tid)

-- | Connects the pipeline to websockets consumers
connectPipelineToWebsockets
  :: (HasView app, HasRequest app, Eq (ViewSelector app SelectedCount))
  => Text
  -> RequestHandler app IO
  -- ^ API handler
  -> QueryHandler (MonoidalMap ClientKey (ViewSelector app SelectedCount)) IO
  -- ^ A way to retrieve more data for each consumer
  -> IO (Recipient (MonoidalMap ClientKey (ViewSelector app SelectedCount)) IO, Snap ())
  -- ^ A way to send data to many consumers and a handler for websockets connections
connectPipelineToWebsockets ver rh qh = do
  (allRecipients, registerRecipient) <- connectPipelineToWebsockets' qh
  return (allRecipients, handleWebsocket ver rh registerRecipient)

-- | Like 'connectPipelineToWebsockets' but returns a Registrar that can
-- be used to construct a handler for a particular client
connectPipelineToWebsockets'
  :: (Monoid (QueryResult q), Group q)
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
  :: ( HasRequest app
     , HasView app
     , q ~ MonoidalMap ClientKey (ViewSelector app SelectedCount)
     , Monoid q', Semigroup q' )
  => Pool Postgresql
  -> RequestHandler app IO
  -> (NotifyMessage -> q' -> IO (QueryResult q'))
  -> QueryHandler q' IO
  -> Pipeline IO q q'
  -> IO (Snap (), IO ())
serveDbOverWebsockets db handleApi handleNotify handleQuery pipe = do
  (getNextNotification, finalizeListener) <- startNotificationListener db
  rec (qh, finalizeFeed) <- feedPipeline (handleNotify <$> getNextNotification) handleQuery r
      (qh', r) <- unPipeline pipe qh r'
      (r', handleListen) <- connectPipelineToWebsockets "" handleApi qh'
  return (handleListen, finalizeFeed >> finalizeListener)

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
     , FunctorMaybe qr
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
    distributeResults v = monoidMap $ Map.mapWithKey (\k _ -> fmapMaybe (Map.lookup k . unMonoidMap) v) $ fold $ fmap unMonoidMap v

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
