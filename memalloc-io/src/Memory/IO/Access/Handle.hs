module Memory.IO.Access.Handle
(
    Handle(..)
) where

import Memory.IO.Prelude

-- | A handle is a label for a resource, eg a file or socket etc, with very few
-- prescribed properties.
--
-- It does not even have to be a reference, since it may not be able to be
-- dereferenced.
--
-- The one property that a handle has over an address is that we can check
-- whether two handles point to the same resource. That is, a handle has the
-- property that, if and only if two handles are equal, then they must point to
-- the same resource.
--
-- This is a stronger guarantee; an address only guarantees that if two
-- addresses are equal, then they must point to the same location, whereas a
-- handle guarantees that if two handles are equal, then they must point to the
-- same resource.
--
-- Eg, while an address can be many-to-one, a handle must be one-to-one.
class Handle h where

    -- | Check whether two handles refer to the same resource.
    hdlEq :: h a -> h a -> Bool

    -- | Hash a handle in a manner suitable for non-cryptographic hash tables.
    hdlHash :: h a -> Int
