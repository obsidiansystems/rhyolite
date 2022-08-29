module Obelisk.View.NonEmptyInterval where

import qualified Data.IntMap.Strict as IntMap
import GHC.Stack

-- | Law: end >= start
-- Empty intervals
data NonEmptyInterval = NonEmptyInterval
  { _nonEmptyInterval_start :: {-# UNPACK #-} !Int -- Values >= this are included
  , _nonEmptyInterval_end :: {-# UNPACK #-} !Int -- Values <= this are included
  }
  deriving (Show, Read, Eq, Ord)

-- | Take the first point from the given interval; if there are any more points left, return them
unconsNonEmptyInterval :: NonEmptyInterval -> (Int, Maybe NonEmptyInterval)
unconsNonEmptyInterval (NonEmptyInterval a b) =
  ( a
  , if a < b
    then Just $ NonEmptyInterval (succ a) b
    else Nothing
  )

allIntsNonEmptyInterval :: NonEmptyInterval
allIntsNonEmptyInterval = NonEmptyInterval minBound maxBound

unsafeNonEmptyInterval :: Int -> Int -> NonEmptyInterval
unsafeNonEmptyInterval = NonEmptyInterval

nonEmptyInterval :: HasCallStack => Int -> Int -> NonEmptyInterval
nonEmptyInterval s e = either error id $ nonEmptyIntervalEither s e

nonEmptyIntervalEither :: Int -> Int -> Either String NonEmptyInterval
nonEmptyIntervalEither s e =
  if e < s
  then Left $ "end " <> show e <> " is before start " <> show s
  else Right $ NonEmptyInterval s e

greaterThanOrEqualInterval :: Int -> NonEmptyInterval
greaterThanOrEqualInterval n = NonEmptyInterval n maxBound

lessThanOrEqualInterval :: Int -> NonEmptyInterval
lessThanOrEqualInterval n = NonEmptyInterval minBound n

lessThanInterval :: Int -> Maybe NonEmptyInterval
lessThanInterval n = if n == minBound then Nothing else Just $ lessThanOrEqualInterval $ pred n

singletonInterval :: Int -> NonEmptyInterval
singletonInterval n = NonEmptyInterval n n

completeInterval :: NonEmptyInterval
completeInterval = NonEmptyInterval minBound maxBound

memberInterval :: Int -> NonEmptyInterval -> Bool
memberInterval n (NonEmptyInterval s e) = n >= s && n <= e

-- | Enumerate all values within the interval
-- Any use of this is probably a performance problem
nonEmptyIntervalToList :: NonEmptyInterval -> [Int]
nonEmptyIntervalToList (NonEmptyInterval a b) = [a..b]

lookupIntMap :: NonEmptyInterval -> IntMap.IntMap a -> IntMap.IntMap a
lookupIntMap (NonEmptyInterval lb ub) xs =
  let (_, lb', xs') = IntMap.splitLookup lb xs
      (xs'', ub', _) = IntMap.splitLookup ub xs'
  in foldMap (IntMap.singleton lb) lb' <> xs'' <> foldMap (IntMap.singleton ub) ub'
