module Memory.IO.Allocator.Arena
(
    ArenaAllocator(..)
) where

import Memory.IO.Prelude

import Memory.IO.Allocator

class (Allocator alr) => ArenaAllocator alr where

    -- | Deallocate all allocations at once
    --
    -- Alt names: deallocAll, reset, clear
    deallocAll :: alr -> IO ()
