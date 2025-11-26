module Memory.IO.Allocator
(
    Allocator(..)
,   Deallocator(..)
,   HasAllocator(..)
) where

import Memory.IO.Prelude

import Memory.IO.Address

class (AddressSpace alr) => Allocator alr where

    data family Layout alr
    data family Allocation alr

    allocate :: alr -> Layout alr -> IO (Address alr)
    initialize :: alr -> Address alr -> (Address alr -> IO a) -> IO a

    -- Data.ByteArray.alloc eg allocRet minus the ret
    allocateInitialized :: alr -> Layout alr -> (Address alr -> IO ()) -> IO (Allocation alr)

    withAllocationAddress :: Allocation alr -> (Address alr -> IO a) -> IO a

class (Allocator alr) => Deallocator alr where

    deallocate :: alr -> Maybe (Layout alr) -> Allocation alr -> IO ()

class (Allocator alr) => HasAllocator t alr where

    allocator :: t -> alr
