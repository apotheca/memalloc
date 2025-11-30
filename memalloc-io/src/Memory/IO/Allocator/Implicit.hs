{-# LANGUAGE ImplicitParams #-}

module Memory.IO.Allocator.Implicit
(
    alloc
,   allocInit
,   allocRet
-- ,   inlineAllocInit
-- ,   create
-- ,   allocAndFreeze
-- ,   unsafeCreate
-- ,   inlineUnsafeCreate
,   empty
) where

import Memory.IO.Prelude

import Memory.IO.Address
import qualified Memory.IO.Allocator as Explicit
import Memory.IO.Layout

type Allocator alr = Explicit.Allocator alr
type Allocation alr = Explicit.Allocation alr

-- NOTE: How is ImplicitParams better / different eg from:
-- class Allocator alr => ProxyAllocator alr where
--     proxyAlloc :: Proxy alr -> Layout alr -> IO (Allocation alr)
--     proxyAllocInit :: Proxy alr -> Layout alr -> (Address alr -> IO ()) -> IO (Allocation alr)

-- TODO: What do these need in terms of INLINE / INLINABLE / NOINLINE?

alloc :: (Allocator alr, ?alr :: alr) => Layout alr -> IO (Allocation alr)
alloc = Explicit.alloc ?alr

allocInit :: (Allocator alr, ?alr :: alr) => Layout alr -> (Address alr -> IO ()) -> IO (Allocation alr)
allocInit = Explicit.allocInit ?alr

allocRet :: (Allocator alr, ?alr :: alr) => Layout alr -> (Address alr -> IO a) -> IO (a, Allocation alr)
allocRet = Explicit.allocRet ?alr

-- create :: (Allocator alr, ?alr :: alr) => Layout alr -> (Address alr -> IO ()) -> IO (Allocation alr)
-- create = Explicit.create ?alr

-- allocAndFreeze :: (Allocator alr, ?alr :: alr) => Layout alr -> (Address alr -> IO ()) -> Allocation alr
-- allocAndFreeze = Explicit.allocAndFreeze ?alr

empty :: (Allocator alr, EmptyLayout alr, ?alr :: alr) => Allocation alr
empty = Explicit.empty ?alr