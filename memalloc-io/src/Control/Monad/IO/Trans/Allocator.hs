{-# LANGUAGE CPP #-}

module Control.Monad.IO.Trans.Allocator
(
    AllocatorT(..)
) where

import Memory.IO.Prelude

import Memory.IO.Allocator

#if defined(USE_MTL)

import Control.Monad.Trans
import Control.Monad.Reader

newtype AllocatorT alr m a = MkAllocatorT
    { runAllocatorT :: ReaderT alr m a
    }
    deriving (Functor, Applicative, Monad, MonadReader alr)

instance MonadTrans (AllocatorT alr) where
    lift = MkAllocatorT . lift

-- TODO: Make MonadAllocator instance for AllocatorT

#endif
