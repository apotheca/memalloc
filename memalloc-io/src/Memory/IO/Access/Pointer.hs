module Memory.IO.Access.Pointer
(
    Pointer(..)
,   UIntPointer(..)
) where

import Memory.IO.Prelude

class Pointer ptr where

    nullPtr :: ptr a
    plusPtr :: ptr a -> Int -> ptr a

class (Pointer ptr) => UIntPointer ptr where

    uintPtr :: ptr a -> Word
    -- Optional, originally named alignPtr
    alignUpPtr :: ptr a -> Int -> ptr a
    -- Optional, originally named minusPtr
    diffPtr :: ptr a -> ptr a -> Int
