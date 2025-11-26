module Memory.IO.Address
(
    AddressSpace(..)
) where

import Memory.IO.Prelude

-- | An address space is a region of memory that can be addressed
class AddressSpace asp where

    -- | An address is a physical or virtual location in memory
    data family Address asp

    addressEqual :: Address asp -> Address asp -> Bool
    addressHash :: Address asp -> Int