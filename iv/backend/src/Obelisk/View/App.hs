module Obelisk.View.App where

import Control.Applicative
import Control.Category
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Constraint.Compose
import Data.Constraint.Empty
import Data.Constraint.Extras
import Data.Functor.Misc
import Data.IORef (atomicModifyIORef', newIORef, readIORef, IORef)
import Data.Some (Some(Some))
import Data.Text (Text)
import Data.These (These(..))
import Database.Beam.AutoMigrate.Annotated
import Database.Beam.AutoMigrate.Generic
import Database.Beam.Postgres (Postgres)
import Database.Beam.Schema.Tables
import GHC.Generics
import Obelisk.Api
import Obelisk.Beam.Constraints
import Obelisk.Beam.DZippable
import Obelisk.Beam.Patch.Db
import Obelisk.Beam.Patch.Decoder
import Obelisk.Beam.Patch.Table
import Obelisk.Beam.TablesOnly
import Obelisk.Beam.TablesV
import Obelisk.View.Collidable
import Obelisk.View.Coverable
import Obelisk.View.Coverage
import Obelisk.View.CoverageMap
import Obelisk.View.Data
import Obelisk.View.DbDriver.Postgres (withDbDriver)
import Obelisk.View.Fan
import Obelisk.View.Interface
import Obelisk.View.Iv.Class
import Obelisk.View.NonEmptyInterval
import Obelisk.View.OccurrenceMap
import Obelisk.View.Postgres
import Obelisk.View.Postgres (runDbIv)
import Obelisk.View.Sequential
import Obelisk.View.SubscriptionMap
import Obelisk.View.Time
import Prelude hiding ((.), id)
import Reflex.Query.Class (QueryResult)
import Rhyolite.Backend.App (ClientKey(..))
import Rhyolite.Backend.App (RequestHandler(..))
import Rhyolite.Backend.WebSocket (getDataMessage, sendEncodedDataMessage, withWebsocketsConnection)
import Rhyolite.WebSocket (TaggedRequest(..), TaggedResponse(..), WebSocketRequest(..))
import Rhyolite.WebSocket (WebSocketResponse(..))
import Snap.Core (Snap)
import qualified Control.Lens as Lens
import qualified Data.Map.Strict as Map
import qualified Data.These.Combinators as These
import qualified Network.WebSockets as WS

data BufferRhyoliteAppState db i = BufferRhyoliteAppState
  { _bufferRhyoliteAppState_cachedDBPatch :: Maybe (OccurrenceMap (QueryResultPatch (TablesV db) TablePatch))
  -- ^ The database notifications at each timestep, in case we need to reuse it to satisfy unresolved subscriptions.
  , _bufferRhyoliteAppState_subscriptions :: SubscriptionMap (Cov (Push i))
  -- ^ outstanding subscriptions.
  , _bufferRhyoliteAppState_readsResponded :: CoverageMap (WithFullCoverage (Cov (Pull i)))
  -- ^ unresolved reads, this fills with readNone and readResponse.
  , _bufferRhyoliteAppState_pendingNotifies :: CoverageMap (Cov (Push i))
  -- ^ notifications we have dispatched to the handler, but not recieved from the handler yet.
  , _bufferRhyoliteAppState_closedTimes :: CoverageMap ()
  }

Lens.makeLenses ''BufferRhyoliteAppState

data ReadDbPatch a where
  ReadDbPatch_Old :: ReadDb a -> ReadDbPatch a
  ReadDbPatch_New :: ReadDb a -> ReadDbPatch a
  ReadDbPatch_Bind :: ReadDbPatch a -> (a -> ReadDbPatch b) -> ReadDbPatch b
  ReadDbPatch_Pure :: a -> ReadDbPatch a
  ReadDbPatch_FMap :: (a -> b) -> ReadDbPatch a -> ReadDbPatch b
  ReadDbPatch_LiftA2 :: (a -> b -> c) -> ReadDbPatch a -> ReadDbPatch b -> ReadDbPatch c

liftOld :: ReadDb a -> ReadDbPatch a
liftOld = ReadDbPatch_Old
liftNew :: ReadDb a -> ReadDbPatch a
liftNew = ReadDbPatch_New

runReadDbPatch :: (AsyncReadDb IO -> IO ()) -> (AsyncReadDb IO -> IO ()) -> ReadDbPatch a -> (a -> IO ()) -> IO ()
runReadDbPatch readOld readNew x0 k0 = go k0 x0
  where
    go :: forall x. (x -> IO ()) -> ReadDbPatch x -> IO ()
    go k = \case
      ReadDbPatch_Old x -> readOld (AsyncReadDb x k)
      ReadDbPatch_New x -> readNew (AsyncReadDb x k)
      ReadDbPatch_Bind x f -> go (\y -> go k $ f y) x
      ReadDbPatch_Pure x -> k x
      ReadDbPatch_FMap f x -> go (k . f) x
      ReadDbPatch_LiftA2 f x y -> do
        res <- newIORef Nothing
        _ <- forkIO $ flip go x $ \x' -> join $ atomicModifyIORef' res $ \case
          Nothing -> (Just (Left x'), pure ())
          Just (Left _) -> error "double fill"
          Just (Right y') -> (Nothing, k $ f x' y')
        flip go y $ \y' -> join $ atomicModifyIORef' res $ \case
          Nothing -> (Just (Right y'), pure ())
          Just (Right _) -> error "double fill"
          Just (Left x') -> (Nothing, k $ f x' y')

instance Functor ReadDbPatch where
  fmap = ReadDbPatch_FMap

instance Applicative ReadDbPatch where
  pure = ReadDbPatch_Pure
  liftA2 = ReadDbPatch_LiftA2

instance Monad ReadDbPatch where
  (>>=) = ReadDbPatch_Bind


type QueryHandler' i m = Cov (Pull i) -> m (Pull i)

-- | A way of attaching to (and later detaching from) a pipeline (e.g., handle
-- client connection and disconnection)
type Registrar' i = IvForwardSequential IO i -> IO (IvBackwardSequential IO i, IO ())

data QueryMorphism' q push pull = InterfaceToQuery
  { _i2q :: q -> These (These (Cov push) (Cov push)) (Cov pull)
  , _push2q :: push -> QueryResult q
  , _pull2q :: pull -> QueryResult q
  }

bufferRhyoliteApp
  :: forall db i.
    ( Coverage (Cov (Push i)), Show (Cov (Push i)), Show (WithFullCoverage (Cov (Push i)))
    , Coverage (Cov (Pull i)), Show (Cov (Pull i)), Show (WithFullCoverage (Cov (Pull i)))
    , ConstraintsForT db (TableHas_ EmptyConstraint)
    , ConstraintsForT db (TableHas_ (ComposeC Semigroup TablePatch))
    , ArgDictT db
    , DZippable db
    )
  => (NonEmptyInterval -> IO ())
  -> (Time -> AsyncReadDb IO -> IO ())
  -> (Cov (Pull i) -> ReadDb (Pull i))
  -> (QueryResultPatch (TablesV db) TablePatch -> Cov (Push i) -> ReadDbPatch (Push i))
  -> IvForward IO i
  -> IO
    ( IvBackward IO i
    , Time -> QueryResultPatch (TablesV db) TablePatch -> IO ()
    )
bufferRhyoliteApp closeTime readAtTime qh nh fwd = do
  var :: IORef (BufferRhyoliteAppState db i) <- newIORef $ BufferRhyoliteAppState
    { _bufferRhyoliteAppState_cachedDBPatch = Nothing
    , _bufferRhyoliteAppState_subscriptions = emptySubscriptionMap
    , _bufferRhyoliteAppState_readsResponded = emptyCoverageMap
    , _bufferRhyoliteAppState_pendingNotifies = emptyCoverageMap
    , _bufferRhyoliteAppState_closedTimes = emptyCoverageMap
    }

  -- reads are sent along immediately as they are recieved; only "finished" status of reads is recorded in state.
  --  the only thing we need to worry about with reads is to call closeTime when we're done reading for that time.
  -- "everything" about subscriptions are recorded, since we get "real" notifications in.
  -- we "can" handle notifications if we have the corresponding entry in cachedDBPatch.
  -- any subscription present is fulfilled as soon as it's dispatched, and immediately added to pending.
  --
  -- we can free cachedDBPatches for times that are unsubscribed and not pending
  -- finally, we can close times for which we have full read coverage, no pending notifies, are unsubscribed
  let processState f = join $ atomicModifyIORef' var $ \st ->
        let st' = f st
            (subs', unsubs') = knownSubscriptionStates $ _bufferRhyoliteAppState_subscriptions st'

            fulfillableSubs = do
              cachedDBPatch <- _bufferRhyoliteAppState_cachedDBPatch st'
              zipOccurenceMapWithCoverageMap (\q p -> Just (q, p)) subs' cachedDBPatch

            fulfillableSubsCov = fmap (occurenceToCoverageMap . fmap fst) fulfillableSubs

            allClosedTimes = coverageMapFullCoveragesOnly (_bufferRhyoliteAppState_readsResponded st')
              `unionCoverage` shiftCoverageMap (-1) (coverageMapFullCoveragesOnly unsubs')
              -- NOTE: we should not need to separately trim out
              -- fulfillableSubsCov from this, because those aren't fulfilled until st'',
              -- and thus not be covered by unsubs'
            newClosedTimes = allClosedTimes `differenceCoverageMaps` _bufferRhyoliteAppState_closedTimes st

            st'' = ($ st')
              $ Lens.over bufferRhyoliteAppState_closedTimes (unionCoverage newClosedTimes)
              . Lens.over bufferRhyoliteAppState_subscriptions (maybe id fulfillSubscriptionMap fulfillableSubsCov)
              . Lens.over bufferRhyoliteAppState_pendingNotifies (maybe id unionCoverage fulfillableSubsCov)
              . Lens.over bufferRhyoliteAppState_cachedDBPatch
                (join . liftA2 (zipOccurenceMapWithCoverageMap (\() -> Just)) (negateCoverage allClosedTimes))

        in (,) st'' $ do

          forM_ fulfillableSubs $ traverseOccurenceMap_ $ \(q, db) t -> do
            runReadDbPatch (readAtTime (pred t)) (readAtTime t) (nh db q) $ \res -> do
              _ivForward_notify fwd res t
              processState $ Lens.over bufferRhyoliteAppState_pendingNotifies (`differenceCoverageMaps` singletonCoverageMap t q)

          traverseWithInterval_CoverageMap (\t () -> closeTime t) newClosedTimes

  pure $ (,)
    IvBackward
      { _ivBackward_subscribe = \q t -> processState $
        Lens.over bufferRhyoliteAppState_subscriptions (fst . subscribeSubscriptionMap t q)
      , _ivBackward_unsubscribe = \q t -> processState $
        Lens.over bufferRhyoliteAppState_subscriptions (fst . unsubscribeSubscriptionMap t q)
      , _ivBackward_subscribeNone = \q -> processState $
        Lens.over bufferRhyoliteAppState_subscriptions (checkpointSubscriptions q)
      , _ivBackward_read = \q t -> readAtTime t $ AsyncReadDb (qh q) $ \qResult -> do
        _ivForward_readResponse fwd qResult t
        processState $ Lens.over bufferRhyoliteAppState_readsResponded (unionCoverage (singletonCoverageMap t $ toWithFullCoverage q))
      , _ivBackward_readNone = \q ->
        processState $ Lens.over bufferRhyoliteAppState_readsResponded (unionCoverage q)
      }
    $ \t db' -> processState $ Lens.over (bufferRhyoliteAppState_cachedDBPatch) ((<>) (Just (singletonOccurrenceMap t db')))


serveDbOverWebsocketsNew
  :: forall db r push pull qWire.
    ( ConstraintsForT db IsTable
    , ConstraintsForT db (TableHas_ EmptyConstraint)
    , ConstraintsForT db (TableHas_ (ComposeC Semigroup TablePatch))
    , ConstraintsFor r ToJSON
    , GZipDatabase Postgres
      (AnnotatedDatabaseEntity Postgres db) (AnnotatedDatabaseEntity Postgres db) (DatabaseEntity Postgres db)
      (Rep (db (AnnotatedDatabaseEntity Postgres db))) (Rep (db (AnnotatedDatabaseEntity Postgres db)))
      (Rep (db (DatabaseEntity Postgres db)))
    , GSchema Postgres db '[] (Rep (db (AnnotatedDatabaseEntity Postgres db)))
    , Database Postgres db
    , Generic (db (DatabaseEntity Postgres db))
    , Generic (db (AnnotatedDatabaseEntity Postgres db))
    , Generic (db (EntityPatchDecoder Postgres))
    , GPatchDatabase (Rep (db (EntityPatchDecoder Postgres)) ())
    , ArgDictT db
    , Collidable push
    , Collidable pull
    , Coverable pull
    , Coverable push
    , Show (Collision push)
    , Show (Collision pull)
    , Show (WithFullCoverage (Cov push))
    , Show (Cov push)
    , Show (Cov pull)
    , Show (WithFullCoverage (Cov pull))
    , Show (db (TableOnly (ComposeMaybe TablePatchInfo)))
    , DPointed db
    , ToJSON (QueryResult qWire)
    , FromJSON qWire
    , FromJSON (Some r)
    , ArgDict ToJSON r
    , Coverage (Cov push)
    , Coverage (Cov pull)
    )
  => ByteString -- Pool Pg.Connection
  -- ^ The database
  -> AnnotatedDatabaseSettings Postgres db
  -> RequestHandler r IO
  -- ^ Handler for the request/response api
  -> (QueryResultPatch (TablesV db) TablePatch -> Cov push -> ReadDbPatch push)
  -- ^ Handler for notifications of changes to the database
  -> (Cov pull -> ReadDb pull)
  -- ^ Handler for new viewselectors
  -> QueryMorphism' qWire push pull
  -- ^ Adapter from the query wire format
  -> IO (Snap (), IO ())
  -- ^ Returns (1) a websocket connection handler function, (2) a finalizer
serveDbOverWebsocketsNew dburi checkedDb rh nh qh fromWire = withDbDriver dburi checkedDb $ \driver -> do
  let initialTime = 1

  handleTimeVar <- newIORef (Left [])

  clientKeyVar <- newIORef (ClientKey 1)

  clientRecipients <- newIORef (Map.empty)

  -- finishedReadsVar <- newIORef (emptyCoverageMap :: CoverageMap (WithFullCoverage (Cov pull)))
  -- finishedNotifyVar <- newIORef (emptyCoverageMap :: CoverageMap (WithFullCoverage (Cov push)))
  -- subscriptionsVar <- newIORef (emptySubscriptionMap :: SubscriptionMap (WithFullCoverage (Cov push)))
  -- cachedDBPatchVar <- newIORef (unsafeEmptyOccurenceMap :: OccurrenceMap (QueryResultPatch (TablesV db) TablePatch))

  -- clientState :: Map ClientKey (ClientState q) <- atomically $ newTVar 
  let
      -- | we aren't "ready" to process new time events until we're almost done
      -- with set-up, so we cache them until we have the "real" handler.
      handleTime t = join $ atomicModifyIORef' handleTimeVar $ \case
        Left ts -> (Left (t:ts), pure ())
        Right f -> (Right f, f t)

      setup
        :: (NonEmptyInterval -> IO ())
        -> (Time -> AsyncReadDb IO -> IO ())
        -> IO ( Time -> QueryResultPatch (TablesV db) TablePatch -> IO ()
              , ( {- IvForward            IO (MapInterface ClientKey ('Interface push pull))
                , -} IvBackwardSequential IO (MapInterface ClientKey ('Interface push pull))
                )
              )
      setup closeTime readAtTime = do
        let fwdSeq :: IvForwardSequential IO (MapInterface ClientKey ('Interface push pull))
            fwdSeq = IvForwardSequential
              { _ivForwardSequential_notify = \pushes -> do
                rcpts <- readIORef clientRecipients
                forM_ (Map.intersectionWith (,) rcpts pushes) $ \(IvForwardSequential f _, x) -> f x
              , _ivForwardSequential_readResponse = \pulls -> do
                rcpts <- readIORef clientRecipients
                forM_ (Map.intersectionWith (,) rcpts pulls) $ \(IvForwardSequential _ f, x) -> f x
              }

        -- we aren't "ready" for outputs yet, we can cache forward data using
        -- workFwd until we have a "real" forward.  It might be possible to use
        -- a bit of MonadFix cleverness and save a couple of pointer derefs,
        -- but this avoids strictness concerns
        (workFwd, setFwd) <- mkBootForwards @_ @(MapInterface ClientKey ('Interface push pull))
        (workBwd, setBwd) <- mkBootBackwards @_ @('Interface push pull)


        (myRead, myReadNone, ourReadResponse :: pull -> Time -> IO ()) <- fanPull
          (_ivBackward_read workBwd)
          (_ivBackward_readNone workBwd)
          (_ivForward_readResponse workFwd)
        (mySub, myUnsub, mySubNone, ourNotify, ourNotifyNone) <- fanPush
          (_ivBackward_subscribe workBwd)
          (_ivBackward_unsubscribe workBwd)
          (_ivBackward_subscribeNone workBwd)
          (_ivForward_notify workFwd)
          (_ivForward_notifyNone workFwd)

        (fwd, bwdSeq, handleTime') <- makeSequential @IO @(MapInterface ClientKey ('Interface push pull)) initialTime
          fwdSeq
          (IvBackward mySub myUnsub myRead myReadNone mySubNone)


        (bwd, notifyAtTime) <- bufferRhyoliteApp closeTime readAtTime qh nh $
          IvForward
            { _ivForward_readResponse = ourReadResponse
            , _ivForward_notify = ourNotify
            , _ivForward_notifyNone = ourNotifyNone
            }
        setBwd bwd

        setFwd fwd
        join $ atomicModifyIORef' handleTimeVar $ \case
          Left xs -> (Right handleTime', forM_ (reverse xs) handleTime')
          Right {} -> error "handleTimeVar is already configured"

        pure (notifyAtTime, bwdSeq)

  runDbIv driver initialTime
    setup
    handleTime $ \(IvBackwardSequential bwdSeq) -> do
      let finalizer = pure ()
          myRegistrar  :: IvForwardSequential IO ('Interface push pull) -> IO (IvBackwardSequential IO ('Interface push pull), IO ())
          myRegistrar rcpt = do
            clientKey <- atomicModifyIORef' clientKeyVar $ \k -> let k' = succ k in (k', k')
            atomicModifyIORef' clientRecipients $ \rcpts -> (Map.union rcpts $ Map.singleton clientKey rcpt, ())
            subsVar <- newIORef Nothing
            pure $ (,)
              IvBackwardSequential
                { _ivBackwardSequential_subscribeUnsubscribeRead = \q -> do
                  forM_ @Maybe (These.justHere q) $ \q' -> atomicModifyIORef' subsVar $ \old ->
                    (old `unionMaybeCoverage` (These.justHere q') `differenceMaybeCoverage` (These.justThere q') , ())
                  bwdSeq $ bimap (bimap (Map.singleton clientKey) (Map.singleton clientKey)) (Map.singleton clientKey) q
                }
              $ do
                join $ atomicModifyIORef' subsVar $ \subs -> (Nothing, forM_ subs $ bwdSeq . This . That . Map.singleton clientKey)
                atomicModifyIORef' clientRecipients $ \rcpts -> (Map.delete clientKey rcpts, ())

      let wsHandled = withWebsocketsConnection (handleWebsocketConnection "TODO:version" fromWire rh (myRegistrar))

      pure (wsHandled, finalizer)

-- ** Connecting a client (via websockets)
-- $connect_client

-- $connect_client
-- Client applications will send queries using a wire format. Typically, this
-- data is transformed (via a 'QueryMorphism') so that it ends up in a shape
-- that supports the group operations that the backend needs to perform
-- (e.g., combining and subtracting queries/interest sets/viewselectors).
--
-- These functions are usually invoked by `serveDbOverWebsockets` or one of the
-- other end-to-end pipelining functions.

-- | Handles a websocket connection given a raw connection. This handler:
--
--  1. Registers the new client
--  2. Separately handles messages coming in over the "api" and "viewselector" channels
--  3. Transforms the incoming wire-format query and produce responses for new inbound queries
handleWebsocketConnection
  :: forall r i qWire.
  (ToJSON (QueryResult qWire), FromJSON qWire, FromJSON (Some r), ConstraintsFor r ToJSON, ArgDict ToJSON r)
  => Text -- ^ Version
  -> QueryMorphism' qWire (Push i) (Pull i)
  -- ^ Query morphism to translate between wire queries and queries with a
  -- reasonable group instance. cf. 'vesselFromWire'
  -> RequestHandler r IO -- ^ Handler for API requests
  -> Registrar' i
  -> WS.Connection
  -> IO ()
handleWebsocketConnection v fromWire rh register conn = do
  let sender = IvForwardSequential
        (sendEncodedDataMessage conn . WebSocketResponse_View @qWire . _push2q fromWire)
        (sendEncodedDataMessage conn . WebSocketResponse_View @qWire . _pull2q fromWire)
  sendEncodedDataMessage conn (WebSocketResponse_Version v :: WebSocketResponse qWire)
  bracket (register sender) snd $ \(IvBackwardSequential vsHandler, _) -> forever $ do
    (wsr :: WebSocketRequest qWire r) <- liftIO $ getDataMessage conn
    case wsr of
      WebSocketRequest_Api (TaggedRequest reqId (Some req)) -> do
        -- TODO: forkIO
        a <- runRequestHandler rh req
        sendEncodedDataMessage conn
          (WebSocketResponse_Api $ TaggedResponse reqId (has @ToJSON req (toJSON a)) :: WebSocketResponse qWire)
      WebSocketRequest_ViewSelector new -> do
        vsHandler $ _i2q fromWire new

