{-# LANGUAGE UndecidableInstances #-}

module Obelisk.View.App where

import Control.Applicative
import Control.Monad.Trans.Reader
import Data.Foldable
import Control.Category
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (bracket, SomeException, handle, throw, finally, Exception)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Constraint.Compose
import Data.Constraint.Empty
import Data.Constraint.Extras
import Data.Functor.Misc
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Some (Some(Some))
import Data.Text (Text)
import Data.These (These(..))
import Data.These.Combinators
import Database.Beam.AutoMigrate
import Database.Beam.Postgres (Postgres)
import Database.Beam.Schema.Tables
import GHC.Generics
import Obelisk.Api
import Obelisk.Beam.Constraints
import Obelisk.Beam.DZippable
import Obelisk.Beam.Patch.Db
import Obelisk.Beam.Patch.Decoder
import Obelisk.Beam.Patch.Decode.Postgres (DecodableDatabase)
import Obelisk.Beam.Patch.Table
import Obelisk.Beam.TablesOnly
import Obelisk.Beam.TablesV
import Obelisk.View.Collidable
import Obelisk.View.Coverable
import qualified Data.Text as T
import Obelisk.View.Coverage
-- import Obelisk.View.CoverageMap
import Obelisk.View.DbDriver.Postgres (withDbDriver)
import Obelisk.View.Interface
import Obelisk.View.NonEmptyInterval
-- import Obelisk.View.OccurrenceMap
import Obelisk.View.Postgres
import Obelisk.View.Sequential
-- import Obelisk.View.SubscriptionMap
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
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set
import qualified Data.These.Combinators as These
import qualified Network.WebSockets as WS
import Data.Align (align)
import Obelisk.View.Vessel
import Data.Vessel.Class
import Data.Vessel.Vessel (Vessel)
import Data.Constraint.Forall
import Data.Vessel.Internal (FlipAp)
import Data.Functor.Identity
import Data.GADT.Compare
import Data.Proxy
import Rhyolite.Vessel.AuthenticatedV

data ClientKeyState i = ClientKeyState
  { _clientKeyState_nextId :: ClientKey
  , _clientKeyState_recipients :: Map.Map ClientKey (IvForwardSequential IO i)
  }


data PendingReaderState
  = PendingReaderState_NoChange -- ^ the new time is announced, and readers should read at this time, but the patch hasn't happened yet.
  | PendingReaderState_ChangeInProgress -- ^ The new patch has been announced.  once all readers for this time have been recieved, the given time should be closed.
  deriving (Show, Eq, Ord)

-- Lens.makeLenses ''BufferRhyoliteAppState
Lens.makeLenses ''ClientKeyState


newtype ReadDbPatch a = ReadDbPatch { unReadDbPatch :: ReaderT (AsyncReadDb IO -> IO (), AsyncReadDb IO -> IO ()) IO a }
    deriving (Functor, Monad, Applicative)

runReadDbPatch :: (AsyncReadDb IO -> IO ()) -> (AsyncReadDb IO -> IO ()) -> ReadDbPatch a -> IO a
runReadDbPatch readOld readNew x0 = runReaderT (unReadDbPatch x0) (readOld, readNew)

callbackToSync :: Exception e => (((Either e a) -> IO ()) -> IO ()) -> IO a
callbackToSync cb = do
    returnVar <- newEmptyMVar
    cb $ putMVar returnVar
    returnValue <- takeMVar returnVar
    either (liftIO . throw) pure returnValue

readDbToAsyncReadDb :: ReadDb a -> (AsyncReadDb IO -> IO ()) -> IO a
readDbToAsyncReadDb req run = callbackToSync $ run . AsyncReadDb req

liftOld :: ReadDb a -> ReadDbPatch a
liftOld r = ReadDbPatch $ asks fst >>= liftIO . readDbToAsyncReadDb r

liftNew :: ReadDb a -> ReadDbPatch a
liftNew r = ReadDbPatch $ asks snd >>= liftIO . readDbToAsyncReadDb r


newtype Pipeline i q = Pipeline
  { runPipeline :: IO
    ( q -> IO (Maybe (These (These (Cov (Push i)) (Cov (Push i))) (Cov (Pull i))))
    , Push i -> IO (Maybe (QueryResult q))
    , Pull i -> IO (Maybe (QueryResult q))
    )
  }

-- | simple pipeline for when the Query and Coverage instances happen to agree.
--
-- Start here and add query morphism to build up to what you need.  This will
-- also do the extra step of keeping track of the "current" query and adjusting
-- the subscribe/unsubscribe/read to account for it.
idCoveragePipeline
  :: forall i q.
     ( Push i ~ Pull i
     , Push i ~ QueryResult q
     , Cov (Push i) ~ q
     , Coverage q
     )
  => Pipeline i q
idCoveragePipeline = Pipeline $ do
  currentSubs <- newIORef (Nothing :: Maybe q)
  pure
    ( \newSub -> atomicModifyIORef' currentSubs $ (,) (Just newSub) . \case
        Nothing -> Just (These (This newSub) newSub)
        Just currentSub ->
          let sub = newSub `differenceCoverage` currentSub
              unsub = currentSub `differenceCoverage` newSub
          in align (align sub unsub) sub
    , pure . Just
    , pure . Just
    )

viewPipeline
  :: forall v f g h.
  ( View v, ForallF Coverage (ViewCov f), HasViewCov f, Eq (v (ViewCov f))
  , QueryResult (v g) ~ v h
  )
  => (forall x. g x -> ViewCov f x) -> (forall x. f x -> h x) -> Pipeline ('Interface (IView v f) (IView v f)) (v g)
viewPipeline toCov toView = Pipeline $ do
  currentSubs <- newIORef (Nothing :: Maybe (IView v (ViewCov f)))
  pure
    ( \newSub ->
        let newSub' = IView $ mapV toCov newSub
        in atomicModifyIORef' currentSubs $ (,) (Just newSub') . \case
          Nothing -> Just $ (These (This newSub') newSub')
          Just currentSub ->
            let sub = newSub' `differenceCoverage` currentSub
                unsub = currentSub `differenceCoverage` newSub'
            in align (align sub unsub) sub
    , \push -> do
        let v = mapV toView $ getIView push
        pure $
          if nullV v
          then Nothing
          else Just v
    , pure . Just . mapV toView . getIView
    )

vesselPipeline
  :: ( Has View k , Has' Eq k (FlipAp QueryV) , GCompare k)
  => Pipeline
    ('Interface (IView (Vessel k) ResultV) (IView (Vessel k) ResultV))
    (Vessel k (Const ()))
vesselPipeline = viewPipeline (\(Const ()) -> QueryV) (\(ResultV x) -> Identity x)

authenticatedVPipeline
  ::
  ( View public , Eq (public QueryV)
  , View private , Eq (private QueryV)
  , View personal , Eq (personal QueryV)
  )
  => Pipeline
    ('Interface (IView (AuthenticatedV public private personal) ResultV) (IView (AuthenticatedV public private personal) ResultV))
    (AuthenticatedV public private personal (Const ()))
authenticatedVPipeline = viewPipeline (\(Const ()) -> QueryV) (\(ResultV x) -> Identity x)

serveDbOverWebsocketsNew
  :: forall db r push pull qWire a.
    ( ConstraintsForT db IsTable
    , ConstraintsForT db (TableHas_ EmptyConstraint)
    , ConstraintsForT db (TableHas_ (ComposeC Semigroup TablePatch))
    , GZipDatabase Postgres
      (DatabaseEntity Postgres db) (DatabaseEntity Postgres db) (DatabaseEntity Postgres db)
      (Rep (db (DatabaseEntity Postgres db))) (Rep (db (DatabaseEntity Postgres db)))
      (Rep (db (DatabaseEntity Postgres db)))
    , Database Postgres db
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
    , Has ToJSON r
    , Coverage (Cov push)
    , Coverage (Cov pull)
    , Show push
    , Show pull
    , Show (db (TableOnly (ComposeMaybe TablePatch)))
    , ConstraintsForT db (TableHas Eq (ComposeMaybe Proxy))
    , DecodableDatabase db
    )
  => (Text -> IO ())
  -- ^ logger
  -> ByteString -- Pool Pg.Connection
  -- ^ The database
  -> DatabaseSettings Postgres db
  -> RequestHandler r IO
  -- ^ Handler for the request/response api
  -> (QueryResultPatch (TablesV db) TablePatch -> Cov push -> ReadDbPatch push)
  -- ^ Handler for notifications of changes to the database
  -> (Cov pull -> ReadDb pull)
  -- ^ Handler for new viewselectors
  -> Pipeline ('Interface push pull) qWire
  -- ^ Adapter from the query wire format
  -> ((Registrar ('Interface (These (QueryResultPatch (TablesV db) TablePatch) push) pull)) -> Snap () -> IO a)
  -- ^ continuation to run while the handler is running; once this callback returns, the system shuts down.
  -> IO a
serveDbOverWebsocketsNew logger dburi db rh nh qh fromWire k =
  serveDbOverWebsocketsNewRaw logger dburi db nh qh (\r -> k r $ withWebsocketsConnection $ handleWebsocketConnection "TODO:version" fromWire rh r)

serveDbOverWebsocketsNewRaw
  :: forall db push pull a.
    ( ConstraintsForT db IsTable
    , ConstraintsForT db (TableHas_ EmptyConstraint)
    , ConstraintsForT db (TableHas_ (ComposeC Semigroup TablePatch))
    , GZipDatabase Postgres
      (DatabaseEntity Postgres db) (DatabaseEntity Postgres db) (DatabaseEntity Postgres db)
      (Rep (db (DatabaseEntity Postgres db))) (Rep (db (DatabaseEntity Postgres db)))
      (Rep (db (DatabaseEntity Postgres db)))
    --, GSchema Postgres db '[] (Rep (db (DatabaseEntity Postgres db)))
    , Database Postgres db
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
    , Coverage (Cov push)
    , Coverage (Cov pull)
    , Show push
    , Show pull
    , Show (db (TableOnly (ComposeMaybe TablePatch)))
    , ConstraintsForT db (TableHas Eq (ComposeMaybe Proxy))
    , DecodableDatabase db
    )
  => (Text -> IO ())
  -- ^ logger
  -> ByteString -- Pool Pg.Connection
  -- ^ The database
  -> DatabaseSettings Postgres db
  -> (QueryResultPatch (TablesV db) TablePatch -> Cov push -> ReadDbPatch push)
  -- ^ Handler for notifications of changes to the database
  -> (Cov pull -> ReadDb pull)
  -- ^ Handler for new viewselectors
  -> (Registrar ('Interface (These (QueryResultPatch (TablesV db) TablePatch) push) pull) -> IO a)
  -- ^ continuation to run while the handler is running; once this callback returns, the system shuts down.
  -> IO a
serveDbOverWebsocketsNewRaw logger dburi db nh qh k = withDbDriver logger dburi db $ \driver -> do
  let initialTime = 1
  -- the "current" time - reads and subscription changes apply to this
  --  , the subscriptions
  timeAndSubsVar <- newIORef @(Time, Maybe (Cov (Map.Map ClientKey (These (QueryResultPatch (TablesV db) TablePatch) push))), Map.Map Time (Int, PendingReaderState))
    (initialTime, Nothing, Map.singleton initialTime (0, PendingReaderState_NoChange))

  let

      -- | we aren't "ready" to process new time events until we're almost done
      -- with set-up, so we cache them until we have the "real" handler.
      handleTime :: Time -> IO ()
      handleTime tNew = do
        logger $ T.unwords ["new time: ", tshow tNew]
        atomicModifyIORef' timeAndSubsVar $ \(_, currentSub, pendingReads) ->
          ((tNew, currentSub, Map.insert tNew (0, PendingReaderState_NoChange) pendingReads), ())

      setup
        :: (NonEmptyInterval -> IO ())
        -> (Time -> AsyncReadDb IO -> IO ())
        -> IO ( Time -> QueryResultPatch (TablesV db) TablePatch -> IO ()
              , Registrar ('Interface (These (QueryResultPatch (TablesV db) TablePatch) push) pull)
              )
      setup closeTime readAtTime = do
        clientRecipients <- newIORef $ ClientKeyState @('Interface (These (QueryResultPatch (TablesV db) TablePatch) push) pull) (ClientKey 1) Map.empty


        let fwdSeq :: IvForwardSequential IO (MapInterface ClientKey ('Interface (These (QueryResultPatch (TablesV db) TablePatch) push) pull))
            fwdSeq = IvForwardSequential
              { _ivForwardSequential_notify = \pushes -> do
                ClientKeyState _ rcpts <- readIORef clientRecipients
                forM_ (Map.intersectionWith (,) rcpts pushes) $ \(IvForwardSequential f _, x) -> f x
              , _ivForwardSequential_readResponse = \pulls -> do
                ClientKeyState _ rcpts <- readIORef clientRecipients
                forM_ (Map.intersectionWith (,) rcpts pulls) $ \(IvForwardSequential _ f, x) -> f x
              }

            -- resolve a specified number of reads at specified times, and if possible, close the corresponding times.
            tryCloseTimes :: Map.Map Time Int -> IO ()
            tryCloseTimes responses = join $ atomicModifyIORef' timeAndSubsVar $ \(t, s, pendingReads) ->
              let pendingReads' =
                    Map.filter (/= (0, PendingReaderState_ChangeInProgress)) $ Map.merge
                      Map.preserveMissing -- unresponded reads; preserve
                      (Map.mapMissing $ \myT _ -> error $ "stale read response at" <> show myT)  -- stale read responses... error!!!
                      (Map.zipWithMatched $ \_ (p, st) p' -> (p - p', st))
                      pendingReads
                      responses
                  newClosedTimes = Set.difference (Map.keysSet pendingReads) (Map.keysSet pendingReads')
              in (,) (t, s, pendingReads') $ do
                logger $ T.unwords ["tryCloseTimes", tshow pendingReads, "responded:", tshow responses, "new closed:", tshow newClosedTimes]
                traverse_ (closeTime . singletonInterval) newClosedTimes

        let notifyAtTime :: Time -> QueryResultPatch (TablesV db) TablePatch -> IO ()
            notifyAtTime tPrev p = join $ atomicModifyIORef' timeAndSubsVar $ \(tNew, currentSubM, pendingReads) ->
              let
                numSubs = maybe 0 (Map.size . Map.mapMaybe justThere) currentSubM -- only count the subs that will cause reads.
                pendingReads' :: Map.Map Time (Int, PendingReaderState)
                pendingReads'
                  = Map.alter (maybe (error "no reader state at tNew" ) (\(i, PendingReaderState_NoChange) -> Just (i + numSubs, PendingReaderState_NoChange))) tNew
                  $ Map.alter (maybe (error "no reader state at tPrev") (\(i, PendingReaderState_NoChange) -> Just (i + numSubs, PendingReaderState_ChangeInProgress))) tPrev pendingReads
              in
                (,) (tNew, currentSubM, pendingReads') $ case currentSubM of
                    Nothing -> tryCloseTimes Map.empty -- we might just be done right away.
                    Just currentSub -> do
                      logger $ T.unwords ["processing at", tshow tPrev, "..", tshow tNew]
                      let covs = Map.mapMaybe justThere currentSub
                          mergedCov = unionCoverages covs
                          decrementTimeRefcounts = tryCloseTimes $ Map.fromList [(tPrev, numSubs), (tNew, numSubs)]
                      flip finally decrementTimeRefcounts $ forM_ mergedCov $ \q -> do
                        res <- runReadDbPatch (readAtTime tPrev) (readAtTime tNew) (nh p q)
                        _ivForwardSequential_notify fwdSeq $ flip Map.mapMaybe covs $ \cov -> That <$> restrictCoverage cov res
                      -- readAtTime is "non-blocking" and will call it's callback later.
                      -- So we need to "wait" until all notifications are actually sent.
                      void $ flip Map.traverseWithKey currentSub $ \clientKey qBoth -> do
                        forM_ (justHere qBoth >>= flip restrictCoverage p) $ \res -> do
                          _ivForwardSequential_notify fwdSeq $ Map.singleton clientKey $ This res

            fanBwd :: IvBackwardSequential IO (MapInterface ClientKey ('Interface (These (QueryResultPatch (TablesV db) TablePatch) push) pull))
            fanBwd = IvBackwardSequential
              { _ivBackwardSequential_subscribeUnsubscribeRead = \subUnsubRead ->
                join $ atomicModifyIORef' timeAndSubsVar $ \case
                  (tCurrent, currentSubs, pendingReads) ->
                    let
                      readsMaybe = justThere subUnsubRead
                      subUnsubs = justHere subUnsubRead
                      newSubs = currentSubs
                        `unionMaybeCoverage` (justHere =<< subUnsubs)
                        `differenceMaybeCoverage` (justThere =<< subUnsubs)
                      pendingReads' = Map.alter (maybe (error "no reader state at tCurrent" ) (\(i, PendingReaderState_NoChange) ->
                        Just (i + length @Maybe readsMaybe, PendingReaderState_NoChange))) tCurrent pendingReads
                    in (,) (tCurrent, newSubs, pendingReads') $ do
                      forM_ @Maybe readsMaybe $ \reads_ ->
                        flip Map.traverseWithKey reads_ $ \clientKey read_ -> do
                          flip finally (tryCloseTimes $ Map.singleton tCurrent 1) $ do
                            result <- callbackToSync $ readAtTime tCurrent . AsyncReadDb (qh read_)
                            _ivForwardSequential_readResponse fwdSeq . Map.singleton clientKey $ result
                      pure ()
              }

        let myRegistrar  :: IvForwardSequential IO ('Interface (These (QueryResultPatch (TablesV db) TablePatch) push) pull) -> IO (IvBackwardSequential IO ('Interface (These (QueryResultPatch (TablesV db) TablePatch) push) pull), IO ())
            myRegistrar rcpt = do
              clientKey <- atomicModifyIORef' clientRecipients $ \(ClientKeyState nextId rcpts) ->
                (ClientKeyState (succ nextId) (Map.insert nextId rcpt rcpts), nextId)
              -- atomicModifyIORef' clientRecipients $ \rcpts -> (Map.union rcpts $ Map.singleton clientKey rcpt, ())
              subsVar <- newIORef Nothing
              pure $ (,)
                IvBackwardSequential
                  { _ivBackwardSequential_subscribeUnsubscribeRead = \q -> do
                    forM_ @Maybe (These.justHere q) $ \q' -> atomicModifyIORef' subsVar $ \old ->
                      (old `unionMaybeCoverage` (These.justHere q') `differenceMaybeCoverage` (These.justThere q') , ())
                    _ivBackwardSequential_subscribeUnsubscribeRead fanBwd $ bimap (bimap (Map.singleton clientKey) (Map.singleton clientKey)) (Map.singleton clientKey) q
                  }
                $ do
                  join $ atomicModifyIORef' subsVar $ \subs -> (Nothing, forM_ subs $ _ivBackwardSequential_subscribeUnsubscribeRead fanBwd . This . That . Map.singleton clientKey)
                  atomicModifyIORef' clientRecipients $ \st -> (Lens.set (clientKeyState_recipients . Lens.at clientKey) Nothing st, ())

        pure (notifyAtTime, Registrar myRegistrar)

  runDbIv logger driver initialTime setup handleTime k


-- ** Connecting a client (via websockets)
-- $connect_client

-- $connect_client
-- Client applications will send queries using a wire format. Typically, this
-- data is transformed (via a 'Pipeline') so that it ends up in a shape
-- that supports the coverage operations
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
  :: forall r i x qWire.
  (ToJSON (QueryResult qWire), FromJSON qWire, FromJSON (Some r), Has ToJSON r)
  => Text -- ^ Version
  -> Pipeline i qWire
  -- ^ Query morphism to translate between wire queries and queries with a
  -- reasonable group instance. cf. 'vesselFromWire'
  -> RequestHandler r IO -- ^ Handler for API requests
  -> Registrar ('Interface (These x (Push i)) (Pull i))
  -> WS.Connection
  -> IO ()
handleWebsocketConnection v fromWire rh register conn = do
  (q2i, push2q, pull2q) <- runPipeline fromWire
  let sender = IvForwardSequential
        (mapM_ (sendEncodedDataMessage conn . WebSocketResponse_View @qWire) <=< maybe (pure Nothing) push2q . justThere)
        (mapM_ (sendEncodedDataMessage conn . WebSocketResponse_View @qWire) <=< pull2q)
  sendEncodedDataMessage conn (WebSocketResponse_Version v :: WebSocketResponse qWire)
  bracket (runRegistrar register sender) snd $ \(IvBackwardSequential vsHandler, _) -> forever $ do
    (wsr :: WebSocketRequest qWire r) <- liftIO $ getDataMessage conn
    case wsr of
      WebSocketRequest_Api (TaggedRequest reqId (Some req)) ->
        -- TODO: don't leak all error messages from the backend to the frontend by blanket sending the text of uncaught errors.
        handle (\(e :: SomeException) -> sendEncodedDataMessage conn ((WebSocketResponse_Api $ TaggedResponse_Error reqId (tshow e)) :: WebSocketResponse qWire) >> throw e) $ do
          -- TODO: forkIO
          a <- runRequestHandler rh req
          sendEncodedDataMessage conn
            (WebSocketResponse_Api $ TaggedResponse reqId (has @ToJSON req (toJSON a)) :: WebSocketResponse qWire)
      WebSocketRequest_ViewSelector new -> do
        q2i new >>= \case
          Nothing -> pure ()
          Just new' -> vsHandler $ bimap (bimap That That) id new'

