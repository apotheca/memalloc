module Memory.IO.Access.Array
(
    Array(..)
,   MutableArray(..)
) where

import Memory.IO.Prelude

-- | An array is a sequentially-indexed multi-reference.
--
-- It provides constant-time access to its elements via index, but does not
-- require those elements to be contiguous in memory.
--
-- Note that operations that affect the size of the array are not required to be
-- constant-time.
class Array arr where

    -- type family Idx arr

    length :: arr a -> Int
    index :: arr a -> Int -> a

class (Array arr) => MutableArray arr where

    storeAt :: arr a -> Int -> a -> IO ()
    updateAt :: arr a -> Int -> (a -> IO a) -> IO ()

-- TODO: StaticArray, DynamicArray, HashedArrayTree implementations