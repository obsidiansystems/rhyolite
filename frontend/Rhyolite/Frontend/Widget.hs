-- | A couple utilities to deal with list of items in which each item can return
-- controls for insertion before and after, and deletion. See 'ListEdit' and
-- 'extensibleListWidget'.

{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rhyolite.Frontend.Widget where

import Control.Monad.Fix
import Data.Map (Map)
import qualified Data.Map as Map
import Reflex.Dom.Core hiding (Delete)

data ListEdit = ListEdit_InsertBefore | ListEdit_Delete | ListEdit_InsertAfter
  deriving (Eq, Ord, Show, Read)

extensibleListWidget
  :: forall t m a b. (DomBuilder t m, MonadHold t m, MonadFix m)
  => Int -- ^ Minimum number of entries (be careful: if this is 0, the entire list is allowed to vanish)
  -> a -- ^ Initial entry for newly inserted items
  -> [a] -- ^ Initial sequence of entries
  -> Event t () -- ^ Add an item at the end of the list
  -> (Dynamic t Int -> a -> m (Event t ListEdit, Dynamic t b))
  -- ^ Widget for a single item which is expected to include
  -- the list editing controls and pass through the resulting events.
  -> m (Dynamic t [b])
extensibleListWidget n x0 xs0 addAtEnd itemWidget = extensibleListWidgetWithSize n x0 xs0 addAtEnd (\d -> itemWidget (fst <$> d))

-- | Like `extensibleListWidget`, but the items know the current size of the whole list, as well as their position.
extensibleListWidgetWithSize
  :: forall t m a b. (DomBuilder t m, MonadHold t m, MonadFix m)
  => Int -- ^ Minimum number of entries (be careful: if this is 0, the entire list is allowed to vanish)
  -> a -- ^ Initial entry for newly inserted items
  -> [a] -- ^ Initial sequence of entries
  -> Event t () -- ^ Add an item at the end of the list
  -> (Dynamic t (Int, Int) -> a -> m (Event t ListEdit, Dynamic t b))
  -- ^ Widget for a single item which is expected to include
  -- the list editing controls and pass through the resulting events.
  -- First argument is (item position, total number of items).
  -> m (Dynamic t [b])
extensibleListWidgetWithSize n x0 xs0 addAtEnd itemWidget = do
  let genIndex :: Map Rational a -> Map Rational a -> Rational
      genIndex us vs =
        case (Map.maxViewWithKey us, Map.minViewWithKey vs) of
          (Nothing       , Nothing       ) -> 0
          (Nothing       , Just ((v, _), _)) -> v - 1
          (Just ((u, _), _), Nothing       ) -> u + 1
          (Just ((u, _), _), Just ((v, _), _)) -> (u + v) / 2
      handleChange :: (Rational, ListEdit) -> Map Rational a -> Map Rational (Maybe a)
      handleChange (k, ListEdit_InsertBefore) xs =
        let (us, x, vs) = Map.splitLookup k xs
            vs' = Map.alter (const x) k vs
            i = genIndex us vs'
        in Map.singleton i (Just x0)
      handleChange (k, ListEdit_Delete) xs =
        if Map.size xs > n
          then Map.singleton k Nothing
          else Map.singleton k (Just x0)
      handleChange (k, ListEdit_InsertAfter) xs =
        let (us, x, vs) = Map.splitLookup k xs
            us' = Map.alter (const x) k us
            i = genIndex us' vs
        in Map.singleton i (Just x0)
      map0 :: Map Rational (Maybe a) = Map.fromList . zip [0..] $ fmap Just xs0
  rec let attachList xs x = case x of
            Nothing -> case Map.maxViewWithKey xs of
              Nothing -> handleChange (0, ListEdit_InsertAfter) xs
              Just ((k, _), _) -> handleChange (k, ListEdit_InsertAfter) xs
            Just x' -> handleChange x' xs
          updateEvent :: Event t (Map Rational (Maybe a)) = attachWith attachList (current listMapD) $ leftmost
            [ Just <$> changeMapE
            , Nothing <$ addAtEnd
            ]
      listMapD :: Dynamic t (Map Rational a) <- fmap (Map.mapMaybe id) <$> foldDyn (\e m -> Map.union e m) map0 updateEvent
      let ixMapD :: Dynamic t (Map Rational Int) = fmap (Map.fromList . (`zip` [0::Int ..]) . Map.keys) listMapD
      resultMapD <- listHoldWithKey (Map.mapMaybe id map0) updateEvent $ \k v -> do
        let ix = fmap (Map.findWithDefault (-1) k) ixMapD
                -- TODO, maybe: figure out why this Map lookup is too strict.
                -- Deleting an item causes a failed lookup, however, I'm not sure it really matters.
        itemWidget ((,) <$> ix <*> fmap length listMapD) v
      let changeMapE = switch . current $ fmap (leftmost . fmap (\(k, v) -> fmap ((,) k) v) . Map.toList . fmap fst) resultMapD
          valuesMapD = joinDynThroughMap $ fmap (fmap snd) resultMapD
          valuesD = fmap Map.elems valuesMapD
  return valuesD
