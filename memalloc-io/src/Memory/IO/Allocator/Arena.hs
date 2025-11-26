module Memory.IO.Allocator.Arena
(
    ArenaAllocator(..)
,   reset
) where

import Memory.IO.Prelude

import Memory.IO.Allocator

class (Allocator alr) => ArenaAllocator alr where

    deallocateAll :: alr -> IO ()

reset :: (ArenaAllocator alr) => alr -> IO ()
reset = deallocateAll
