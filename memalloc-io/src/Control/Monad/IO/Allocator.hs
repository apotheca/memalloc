{-# LANGUAGE CPP #-}

module Control.Monad.IO.Allocator
(
    MonadAllocator(..)
) where

import Memory.IO.Prelude

import Memory.IO.Allocator

#if defined(USE_MTL)
import Control.Monad.Reader
#endif

class (Monad m, Allocator alr) => MonadAllocator alr m where

    getAllocator :: m alr

#if defined(USE_MTL)

-- Is this appropriate?
instance (Monad m, Allocator alr) => MonadAllocator alr (ReaderT alr m) where
    getAllocator = ask

#endif
