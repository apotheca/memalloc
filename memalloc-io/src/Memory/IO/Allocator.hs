{-# LANGUAGE ImplicitParams #-}

module Memory.IO.Allocator
(
    Allocator(..)
,   Deallocator(..)
,   Reallocator(..)
,   HasAllocator(..)
,   allocRet
,   inlineAllocInit
,   create
,   allocAndFreeze
,   unsafeCreate
,   inlineUnsafeCreate
,   empty
) where

import Memory.IO.Prelude

import Memory.IO.Address
import Memory.IO.Layout

-- | An allocator reserves a block or region of memory for a particular use.
--
-- This is in comparison to a layout allocator, which only allocates individual
-- addresses.
--
-- NOTE: This class was a little busy, and got split into Allocator and
-- LayoutAllocator. I may further split off `withAddress` into its own
-- class.
class (LayoutSpace alr) => Allocator alr where

    -- | An allocation wraps an address, and may contain additional data about
    -- what is stored there, eg size, alignment, type, refcount, etc.
    data family Allocation alr

    -- | Allocate a new region of memory with a specific layout.
    alloc :: alr -> Layout alr -> IO (Allocation alr)
    alloc alr lo = allocInit alr lo (\_ -> pure ())

    -- | Allocate a new region of memory with specific layout, and initialize it.
    --
    -- Equivalent to Data.ByteArray.alloc
    allocInit :: alr -> Layout alr -> (Address alr -> IO ()) -> IO (Allocation alr)
    allocInit alr lo f = do
        aln <- alloc alr lo
        withAddress aln f
        pure aln

    -- | Temporarily access the underlying address of an allocation.
    withAddress :: Allocation alr -> (Address alr -> IO a) -> IO a

    {-# MINIMAL (alloc | allocInit), withAddress #-}

class (Allocator alr) => Deallocator alr where

    -- | Deallocate an allocation. The layout may be provided as a hint; if this
    -- argument proves unproductive, it may be removed in a future version. If
    -- so, the Reallocator class will also need to be changed to match.
    dealloc
        :: alr
        -> Maybe (Layout alr)
        -> Allocation alr
        -> IO ()

class (Deallocator alr) => Reallocator alr where
    realloc
        :: alr
        -> Maybe (Layout alr)
        -> Layout alr
        -> Allocation alr
        -> IO (Allocation alr)
    realloc alr oldLo lo aln = do
        newAln <- alloc alr lo
        dealloc alr oldLo aln
        pure newAln

class (Allocator alr) => HasAllocator t alr where

    allocator :: t -> alr

-- allocRet :: (ByteArray ba) => Int -> (Ptr p -> IO a) -> IO (a, ba)
allocRet
    :: (Allocator alr)
    => alr
    -> Layout alr
    -> (Address alr -> IO a)
    -> IO (a, Allocation alr)
allocRet alr lo f = do
    aln <- allocInit alr lo (\_ -> pure ())
    a <- withAddress aln f
    pure (a, aln)

inlineAllocInit
    :: (Allocator alr)
    => alr
    -> Layout alr 
    -> (Address alr -> IO ())
    -> IO (Allocation alr)
inlineAllocInit = allocInit
{-# INLINE inlineAllocInit #-}

create :: (Allocator alr) => alr -> Layout alr -> (Address alr -> IO ()) -> IO (Allocation alr)
create alr lo f = inlineAllocInit alr lo f

allocAndFreeze :: (Allocator alr) => alr -> Layout alr -> (Address alr -> IO ()) -> Allocation alr
allocAndFreeze alr lo f = unsafeDoIO (inlineAllocInit alr lo f)
{-# NOINLINE allocAndFreeze #-}

unsafeCreate :: (Allocator alr) => alr -> Layout alr -> (Address alr -> IO ()) -> Allocation alr
unsafeCreate alr lo f = unsafeDoIO (inlineAllocInit alr lo f)
{-# NOINLINE unsafeCreate #-}

inlineUnsafeCreate :: (Allocator alr) => alr -> Layout alr -> (Address alr -> IO ()) -> Allocation alr
inlineUnsafeCreate !alr !lo f = unsafeDoIO (inlineAllocInit alr lo f)
{-# INLINE inlineUnsafeCreate #-}

empty :: (Allocator alr, EmptyLayout alr) => alr -> Allocation alr
empty alr = unsafeDoIO (inlineAllocInit alr emptyLayout $ \_ -> return ())

-- TODO: These allocation typeclasses are not fully developed yet, we will flesh
-- them out later.

-- class Initializable r where
--     -- ???

-- class Finalizable r where
--     finalize :: r a -> IO ()

-- class Allocatable r where
--     -- ???

-- class Freeable r where
--     free :: r a -> IO ()

-- class Copyable r where
--     copyTo :: r a -> r a -> IO ()

-- class (Copyable r) => Movable r where
--     moveTo :: r a -> r a -> IO ()
