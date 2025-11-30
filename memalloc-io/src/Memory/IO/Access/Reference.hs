module Memory.IO.Access.Reference
(
    Reference(..)
,   MutableReference(..)
) where

import Memory.IO.Prelude

-- | A reference allows you to access the value it points to.
--
-- At a low level, dereferencing means copying the value from one address to
-- another, potentially to a different address space altogether. In most
-- languages, this usually means copying the value to a register or the stack.
--
-- In Haskell, this means yielding a lifted value.
class Reference r where

    -- | Access the value stored in the reference.
    --
    -- A more convenient name than 'dereference'
    --
    -- Alt names: deref, read, get, peek
    load :: r a -> IO a

class (Reference r) => MutableReference r where

    -- | Store a value in the reference.
    --
    -- A more convenient name than 'reference'
    --
    -- Alt names: refer, assign, set, poke
    store :: r a -> a -> IO ()

    -- | Modify the value stored in the reference with an effectful function.
    --
    -- Alt names: modify, mutate
    update :: r a -> (a -> IO a) -> IO ()
