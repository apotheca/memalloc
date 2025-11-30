module Memory.IO.Access.Pointer
(
    Pointer(..)
,   UIntPointer(..)
) where

import Memory.IO.Prelude

-- | A pointer is a reference that is able to perform pointer arithmetic on its
-- address.
--
-- Note that it does not imply Reference, because an instance may instead imply 
-- Array or some other access mechanism.
-- 
-- A pointer is not the same as an array, because a pointer address may have
-- multiple successor addresses, depending on the type of the pointer, and
-- because a pointer may access multiple addresses at the same time as a single
-- unit. A pointer can be used to efficiently implement an array, but an array
-- can only emulate a pointer.
class Pointer ptr where

    -- | The null pointer, it doesn't point to anything.
    --
    -- A null pointer is usually implemented as a special address in the address
    -- space, often 0 but not always.
    --
    -- The null pointer is always equal to itself.
    nullPtr :: ptr a

    -- | Increment a pointer address by some number of elements, as if by the
    -- repeated application of some 'succPtr' operation. This does not guarantee
    -- that the resulting pointer is valid.
    --
    -- Two pointers with the same address but different pointer types may have
    -- different successors
    plusPtr :: ptr a -> Int -> ptr a
    
    -- | Increment a pointer to the next address. This does not guarantee that
    -- the resulting pointer is valid.
    --
    -- Note that this is not necessarily equivalent to 'addr + 1', even if the
    -- address can be cast to an integer. It is usually equivalent to
    -- 'addr + step', where step is the aligned size of the pointer type.
    -- succPtr :: ptr a -> ptr a

    -- TODO: Do we need ptrEq? Ptr equality does involve the pointer type, so
    -- not just address equality.


-- | An integer pointer can represent its address as an unsigned integer.
--
-- This allows for the pointer to be cast to a flat address space, and for
-- pointers to be aligned or subtracted from one another - both require being
-- able to inspect the address as an integer, compared to `plusPtr` which only
-- puts a number into the address space rather than getting one out of it.
class (Pointer ptr) => UIntPointer ptr where

    -- | Cast the pointer's address to an unsigned integer.
    uintPtr :: ptr a -> Word

    -- Optional, originally named alignPtr
    -- NOTE: This could / should be generalized to:
    --     alignLayout :: Address asp -> Layout asp -> Address asp
    alignUpPtr :: ptr a -> Int -> ptr a
    -- Optional, originally named minusPtr
    diffPtr :: ptr a -> ptr a -> Int


-- | A pointer can perform pointer arithmetic, meaning if its address can be
-- converted to an integer, it can be interpreted as some set of bits.
--
-- Sometimes, the pointer / word size can be very large, eg 64 bits, large
-- enough that we can't really use it all, and so many systems limit the
-- address space to a smaller size, eg 48 bits which for a byte-addressable
-- system of 256 terabytes.
--
-- A tagged pointer stores extra data in the unused bits of the pointer.  This
-- is commonly used to store a type or constructor tag or some other metadata.
-- class (UIntPointer ptr) => TaggedPointer ptr where
--     data family Tag ptr :: * -> *
--     ptrTag :: ptr a -> Tag ptr a
--     untaggedAddr :: ptr a -> Address asp
