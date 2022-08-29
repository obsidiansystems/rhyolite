module Obelisk.View.These1 where

import Data.Align
import Data.These
import Data.Typeable
import GHC.Generics (Generic)

data These1 f g a
  = This1 (f a)
  | That1 (g a)
  | These1 (f a) (g a)
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show, Typeable, Generic)

these1 :: (f a -> c) -> (g a -> c) -> (f a -> g a -> c) -> These1 f g a -> c
these1 a2c b2c ab2c = \case
  This1 a -> a2c a
  That1 b -> b2c b
  These1 a b -> ab2c a b

justHere1 :: These1 f g a -> Maybe (f a)
justHere1 = \case
  This1 a -> Just a
  That1 _ -> Nothing
  These1 a _ -> Just a

justThere1 :: These1 f g a -> Maybe (g a)
justThere1 = \case
  This1 _ -> Nothing
  That1 b -> Just b
  These1 _ b -> Just b

align1 :: Align f => f (g a) -> f (h a) -> f (These1 g h a)
align1 = alignWith (these This1 That1 These1)

bimapThese1 :: (f a -> h b) -> (g a -> i b) -> These1 f g a -> These1 h i b
bimapThese1 f2h g2i = \case
  This1 a -> This1 $ f2h a
  That1 b -> That1 $ g2i b
  These1 a b -> These1 (f2h a) (g2i b)

combineThese1Maybe
  :: (Maybe (f a) -> Maybe (f' a') -> Maybe (f'' a''))
  -> (Maybe (g a) -> Maybe (g' a') -> Maybe (g'' a''))
  -> These1 f g a
  -> These1 f' g' a'
  -> Maybe (These1 f'' g'' a'')
combineThese1Maybe opA opB x y = align1
  (opA (justHere1 x) (justHere1 y))
  (opB (justThere1 x) (justThere1 y))

unionThese1With
  :: (f a -> f a -> f a)
  -> (g a -> g a -> g a)
  -> These1 f g a
  -> These1 f g a
  -> These1 f g a
unionThese1With opA opB = \case
  This1 xa -> \case
    This1 ya -> This1 $ opA xa ya
    That1 yb -> These1 xa yb
    These1 ya yb -> These1 (opA xa ya) yb
  That1 xb -> \case
    This1 ya -> These1 ya xb
    That1 yb -> That1 $ opB xb yb
    These1 ya yb -> These1 ya (opB xb yb)
  These1 xa xb -> \case
    This1 ya -> These1 (opA xa ya) xb
    That1 yb -> These1 xa (opB xb yb)
    These1 ya yb -> These1 (opA xa ya) (opB xb yb)
