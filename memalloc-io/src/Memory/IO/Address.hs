module Memory.IO.Address
(
    AddressSpace(..)
) where

import Memory.IO.Prelude

-- | An address space is anything that defines a range of addresses. This
-- address space is not required to be dense, contiguous, or even ordered.
--
-- The most commonly encountered address space is the 64-bit flat Von
-- Neumann virtual address space, followed quickly by the IP address spaces.

-- An address has the property that, if two addresses are equal, then they must
-- point to the same location.
--
-- See: https://en.wikipedia.org/wiki/Address_space for the general concept.
class AddressSpace asp where

    -- | An address is used to locate data eg in physical or virtual memory.
    data family Address asp

    -- | Check if two addresses are equal. 
    --
    -- This is not the same thing as asking whether two addresses point to the 
    -- same location. Some address spaces allow for multiple addresses to
    -- resolve to the same location, eg segmented / far pointers.
    addrEq :: Address asp -> Address asp -> Bool

    -- | Hash an address in a manner suitable for non-cryptographic hash tables.
    --
    -- This function is purely for convenience, since we cannot assume that
    -- Address has an Ord instance.
    addrHash :: Address asp -> Int
