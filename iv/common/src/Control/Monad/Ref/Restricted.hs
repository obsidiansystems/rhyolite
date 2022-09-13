{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- |
-- Module      :  Control.Monad.Ref
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2014
-- License     :  BSD-style
-- Maintainer  :  Geoffrey Mainland <mainland@cs.drexel.edu>
--
-- Stability   :  experimental
-- Portability :  non-portable

module Control.Monad.Ref.Restricted (
    MonadRef(..),
    MonadAtomicRef(..)
  ) where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar,
                                    newTVar,
                                    readTVar,
                                    writeTVar)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Error (ErrorT, Error)
#if MIN_VERSION_transformers(0,4,0)
import Control.Monad.Trans.Except (ExceptT)
#endif /* MIN_VERSION_transformers(0,4,0) */
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Lazy as Lazy (StateT)
import Control.Monad.Trans.State.Strict as Strict (StateT)
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import Control.Monad.Trans.Writer.Strict as Strict (WriterT)
import Control.Monad.Trans.Class (lift)
import Data.IORef (IORef,
#if MIN_VERSION_base(4,6,0)
                   atomicModifyIORef',
                   modifyIORef',
#endif /* MIN_VERSION_base(4,6,0) */
                   atomicModifyIORef,
                   modifyIORef,
                   newIORef,
                   readIORef,
                   writeIORef)
import Data.Monoid (Monoid)
import Data.STRef (STRef,
#if MIN_VERSION_base(4,6,0)
                   modifySTRef',
#endif /* MIN_VERSION_base(4,6,0) */
                   modifySTRef,
                   newSTRef,
                   readSTRef,
                   writeSTRef)
import Data.Constraint (Constraint)
import GHC.Stack

-- |The 'MonadRef' type class abstracts over the details of manipulating
-- references, allowing one to write code that uses references and can operate
-- in any monad that supports reference operations.

class (Monad m) => MonadRef m where
    type RefData m :: * -> Constraint
    type Ref m :: * -> *

    -- |Create a new reference
    newRef    :: (HasCallStack, RefData m a) => a -> m (Ref m a)
    -- |Read the value of a reference
    readRef   :: RefData m a => Ref m a -> m a
    -- |Write a new value to a reference
    writeRef  :: RefData m a => Ref m a -> a -> m ()
    -- |Mutate the contents of a reference
    modifyRef :: RefData m a => Ref m a -> (a -> a) -> m ()
    modifyRef r f = readRef r >>= writeRef r . f
    -- |Strict version of 'modifyRef'
    modifyRef' :: RefData m a => Ref m a -> (a -> a) -> m ()
    modifyRef' r f = readRef r >>= \x -> let x' = f x in x' `seq` writeRef r x'

class (MonadRef m) => MonadAtomicRef m where
    -- |Atomically mutate the contents of a reference
    atomicModifyRef :: RefData m a => Ref m a -> (a -> (a, b)) -> m b
    -- |Strict version of atomicModifyRef. This forces both the value stored in
    -- the reference as well as the value returned.
    atomicModifyRef' :: RefData m a => Ref m a -> (a -> (a, b)) -> m b
    atomicModifyRef' r f = do
        b <- atomicModifyRef r
                (\x -> let (a, b) = f x
                        in (a, a `seq` b))
        b `seq` return b

class c => ConstC c a where
instance c => ConstC c a where

instance MonadRef (ST s) where
    type RefData (ST s) = ConstC ()
    type Ref (ST s) = STRef s

    newRef     = newSTRef
    readRef    = readSTRef
    writeRef   = writeSTRef
    modifyRef  = modifySTRef
#if MIN_VERSION_base(4,6,0)
    modifyRef' = modifySTRef'
#endif /* MIN_VERSION_base(4,6,0) */

instance MonadRef IO where
    type RefData IO = ConstC ()
    type Ref IO = IORef

    newRef     = newIORef
    readRef    = readIORef
    writeRef   = writeIORef
    modifyRef  = modifyIORef
#if MIN_VERSION_base(4,6,0)
    modifyRef' = modifyIORef'
#endif /* MIN_VERSION_base(4,6,0) */

instance MonadRef STM where
    type RefData STM = ConstC ()
    type Ref STM = TVar

    newRef    = newTVar
    readRef   = readTVar
    writeRef  = writeTVar

instance MonadRef m => MonadRef (ContT r m) where
    type RefData (ContT r m) = RefData m
    type Ref (ContT r m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance (Error e, MonadRef m) => MonadRef (ErrorT e m) where
    type RefData (ErrorT e m) = RefData m
    type Ref (ErrorT e m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

#if MIN_VERSION_transformers(0,4,0)
instance (MonadRef m) => MonadRef (ExceptT e m) where
    type RefData (ExceptT e m) = RefData m
    type Ref (ExceptT e m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f
#endif /* MIN_VERSION_transformers(0,4,0) */

instance MonadRef m => MonadRef (IdentityT m) where
    type RefData (IdentityT m) = RefData m
    type Ref (IdentityT m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance MonadRef m => MonadRef (ListT m) where
    type RefData (ListT m) = RefData m
    type Ref (ListT m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance MonadRef m => MonadRef (MaybeT m) where
    type RefData (MaybeT m) = RefData m
    type Ref (MaybeT m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance MonadRef m => MonadRef (ReaderT r m) where
    type RefData (ReaderT r m) = RefData m
    type Ref (ReaderT r m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance MonadRef m => MonadRef (Lazy.StateT s m) where
    type RefData (Lazy.StateT s m) = RefData m
    type Ref (Lazy.StateT s m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance MonadRef m => MonadRef (Strict.StateT s m) where
    type RefData (Strict.StateT s m) = RefData m
    type Ref (Strict.StateT s m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance (Monoid w, MonadRef m) => MonadRef (Lazy.WriterT w m) where
    type RefData (Lazy.WriterT w m) = RefData m
    type Ref (Lazy.WriterT w m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance (Monoid w, MonadRef m) => MonadRef (Strict.WriterT w m) where
    type RefData (Strict.WriterT w m) = RefData m
    type Ref (Strict.WriterT w m) = Ref m

    newRef     r   = lift $ newRef     r
    readRef    r   = lift $ readRef    r
    writeRef   r x = lift $ writeRef   r x
    modifyRef  r f = lift $ modifyRef  r f
    modifyRef' r f = lift $ modifyRef' r f

instance MonadAtomicRef IO where
    atomicModifyRef = atomicModifyIORef
#if MIN_VERSION_base(4,6,0)
    atomicModifyRef' = atomicModifyIORef'
#endif /* MIN_VERSION_base(4,6,0) */

instance MonadAtomicRef STM where
    atomicModifyRef r f = do x <- readRef r
                             let (x', y) = f x
                             writeRef r x'
                             return y

instance MonadAtomicRef m => MonadAtomicRef (ContT r m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance (Error e, MonadAtomicRef m) => MonadAtomicRef (ErrorT e m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance MonadAtomicRef m => MonadAtomicRef (IdentityT m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance MonadAtomicRef m => MonadAtomicRef (ListT m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance MonadAtomicRef m => MonadAtomicRef (MaybeT m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance MonadAtomicRef m => MonadAtomicRef (ReaderT r m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance MonadAtomicRef m => MonadAtomicRef (Lazy.StateT s m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance MonadAtomicRef m => MonadAtomicRef (Strict.StateT s m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance (Monoid w, MonadAtomicRef m) => MonadAtomicRef (Lazy.WriterT w m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f

instance (Monoid w, MonadAtomicRef m) => MonadAtomicRef (Strict.WriterT w m) where
    atomicModifyRef  r f = lift $ atomicModifyRef  r f
    atomicModifyRef' r f = lift $ atomicModifyRef' r f
