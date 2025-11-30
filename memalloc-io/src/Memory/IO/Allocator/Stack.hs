module Memory.IO.Allocator.Stack
(
    StackAllocator(..)
) where

import Memory.IO.Prelude
import Memory.IO.Allocator

class (Allocator alr) => StackAllocator alr where

    pop :: alr -> (Allocation alr -> IO a) -> IO a

-- TODO: Once Allocator is finalized, push is trivial
-- push :: (StackAllocator alr) => alr -> Layout alr -> IO (Allocation alr)
-- push = ...

-- TODO: Callstack / Frame allocator
-- class (StackAllocator alr) => CallstackAllocator alr where
--
--     rewind :: alr -> (Frame alr -> IO a) -> IO a