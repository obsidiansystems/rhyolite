
module Obelisk.View.Debug where

import Control.Lens.Indexed
import Obelisk.View.Coverable
import Obelisk.View.Interface

import Debug.Trace
import Obelisk.View.Sequential

withTraceM :: Monad m => String -> m a -> m a
withTraceM s a = do
  traceM s
  a

withTraceShow :: Show a => String -> a -> a
withTraceShow s a = trace (showString s . showString ": " . showsPrec 10 a $ "") a

traceForwardSequential
  :: ( Monad m
     , Show (Push a)
     , Show (Pull a)
     )
  => String
  -> IvForwardSequential m a
  -> IvForwardSequential m a
traceForwardSequential s f = IvForwardSequential
  { _ivForwardSequential_notify = \n       -> withTraceM (showString s . showString ": _ivForwardSequential_notify f "       . showsPrec 11 n $ "") $ _ivForwardSequential_notify f n
  , _ivForwardSequential_readResponse = \v -> withTraceM (showString s . showString ": _ivForwardSequential_readResponse f " . showsPrec 11 v $ "") $ _ivForwardSequential_readResponse f v
  }

traceBackwardSequential
  :: ( Monad m
     , Show (Cov (Push a))
     , Show (Cov (Pull a))
     )
  => String
  -> IvBackwardSequential m a
  -> IvBackwardSequential m a
traceBackwardSequential s b = IvBackwardSequential
  { _ivBackwardSequential_subscribeUnsubscribeRead = \q -> withTraceM (showString s . showString ": _ivBackwardSequential_subscribeUnsubscribeRead b " . showsPrec 11 q $ "") $ _ivBackwardSequential_subscribeUnsubscribeRead b q
  }
