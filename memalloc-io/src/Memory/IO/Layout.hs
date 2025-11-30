module Memory.IO.Layout
(
    LayoutSpace(..)
,   EmptyLayout(..)
) where

import Memory.IO.Prelude

import Memory.IO.Address

-- | A layout space is a primitive allocator that can allocate or assign
-- an available, sufficiently-capable address according to a given layout.
--
-- NOTE: This was originally called LayoutAllocator; it needs some
-- disambiguation.
class (AddressSpace alr) => LayoutSpace alr where

    -- | What a layout is, is deliberately left abstract.
    --
    -- Common layout properties range from simple size and alignment to more
    -- complex layouts such as Struct-of-Arrays for ECS or GPU buffers.
    data family Layout alr

    -- | Assign an available address according to the layout.
    layout :: alr -> Layout alr -> IO (Address alr)
    layout alr lo = layoutInit alr lo pure

    -- | Assign an available address according to the layout, and initialize it.
    layoutInit :: alr -> Layout alr -> (Address alr -> IO a) -> IO a
    layoutInit alr lo f = do
        addr <- layout alr lo
        f addr
    -- Or, in point-free style:
    -- layoutInit alr lo f = layout alr lo >>= f

    {-# MINIMAL layout | layoutInit #-}

-- class LayoutDeallocator alr where
--
--     deallocLayout :: alr -> Layout alr -> Address alr -> IO ()


class EmptyLayout alr where
    emptyLayout :: Layout alr
