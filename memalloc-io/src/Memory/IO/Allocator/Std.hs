module Memory.IO.Allocator.Std
(   StdAllocator(..)
,   Layout(StdLayout)
,   Allocation(StdAllocation)
,   stdAddressWord
,   stdLayoutSize
,   stdAllocationPtr
) where

import Memory.IO.Prelude

import Memory.IO.Address
import Memory.IO.Allocator
import Memory.IO.Layout
import Memory.IO.Access.Reference
import Memory.IO.Access.Pointer

import Data.Void

import Foreign.Marshal.Alloc
import Foreign.Ptr

data StdAllocator = StdAllocator

-- We do a funky little dance here because the `Ptr Addr#` is not exposed.
--
-- Its fine, we'll just use a newtype over Word via WordPtr.
ptrToStdAddress :: Ptr a -> Address StdAllocator
ptrToStdAddress ptr = StdAddress $ wordPtrWord $ ptrToWordPtr ptr where
    wordPtrWord (WordPtr wrd) = wrd

stdAddressToPtr :: Address StdAllocator -> Ptr a
stdAddressToPtr (StdAddress wrd) = wordPtrToPtr $ WordPtr wrd

instance AddressSpace StdAllocator where

    newtype instance Address StdAllocator = StdAddress
        { stdAddressWord :: Word
        }
        deriving newtype (Eq, Ord)

    addrEq = (==)
    addrHash = undefined -- hash

instance LayoutSpace StdAllocator where

    newtype instance Layout StdAllocator = StdLayout
        { stdLayoutSize :: Int
        }

    layout _ (StdLayout size) = ptrToStdAddress <$> mallocBytes size

instance Allocator StdAllocator where

    newtype instance Allocation StdAllocator = StdAllocation
        { stdAllocationPtr :: Ptr Void
        }

    alloc alr lo = do
        addr <- layout alr lo
        pure $ StdAllocation $ stdAddressToPtr addr

    withAddress (StdAllocation ptr) f = f (ptrToStdAddress ptr)

instance Deallocator StdAllocator where
    dealloc _ _ (StdAllocation ptr) = free ptr

instance Reallocator StdAllocator where
    realloc _ _ lo (StdAllocation ptr) = StdAllocation <$> reallocBytes ptr (stdLayoutSize lo)

-- Now herein lies the problem, because we want to do this:
--
-- instance Reference (Allocation StdAllocator) where
--     load (StdAllocation ptr) = peek ptr
--
-- instance Pointer (Allocation StdAllocator) where
--     nullPtr (StdAllocation ptr) = nullPtr ptr
--     plusPtr (StdAllocation ptr) offset = StdAllocation $ plusPtr ptr offset
--
-- But we can't, because Allocation is `*` but Reference / Pointer is `* -> *`
-- It needs to be parametric so we can eg allocate a 'Ptr a' but it needs to not
-- be so we can eg allocate a 'ByteString', which hides a 'Ptr Word8'
--
-- What I might need to do is make *Allocator* parametric,eg:
{-
class (LayoutSpace alr) => PAllocator alr a where

    -- | An allocation wraps an address, and may contain additional data about
    -- what is stored there, eg size, alignment, type, refcount, etc.
    data family PAllocation alr :: * -> *

    -- | Allocate a new region of memory with a specific layout.
    palloc :: alr -> Layout alr -> IO (PAllocation alr a)

instance PAllocator StdAllocator Void where

    newtype instance PAllocation StdPAllocator a = StdPAllocation
        { stdAllocationPtr :: Ptr a
        }

    alloc alr lo = do
        addr <- layout alr lo
        pure $ StdPAllocation $ stdAddressToPtr addr
-}
-- Then, we could keep the original as MonoAllocator if we want, but I don't
-- think that it's necessary, because this works for any of the following:
{-
-- Monomorphic
newtype instance PAllocation VoidPtrAllocator Void
    = MkVoidPtrAllocation (Ptr Void)
-- Polymorphic
newtype instance PAllocation StdPtrAllocator a
    = MkStdPtrAllocation (Ptr a)
-- Phantom
newtype instance PAllocation ByteStringAllocator ByteString
    = MkByteStringAllocation ByteString
-}
-- This is because references are often phantom eg data Ref a = Ref Foo, while
-- pointers are polymorphic / castable, and arrays are monomorphic / not
-- castable. This works to represent all three.
-- NOTE: It is also how we produce *wrapped* allocations, eg a ByteString is
-- secretly wrapping a Ptr Word8 which is secretly wrapping an Addr#.