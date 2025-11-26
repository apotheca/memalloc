{-# LANGUAGE CPP #-}

module Memory.IO.Prelude
( module Prelude
, module Data.Bool
, module Data.Maybe
, module Data.Proxy
-- #if defined(USE_MTL)
-- , module Control.Monad.Reader
-- #endif
, unsafeDoIO
) where

import Prelude

import Data.Bool
import Data.Maybe
import Data.Proxy

-- #if defined(USE_MTL)
-- import Control.Monad.Reader
-- #endif

import System.IO.Unsafe

-- | Used to perform IO for allocations and ffi in pure code.
--
-- Taken straight from `memory`.
unsafeDoIO :: IO a -> a
#if __GLASGOW_HASKELL__ > 704
unsafeDoIO = unsafeDupablePerformIO
#else
unsafeDoIO = unsafePerformIO
#endif
