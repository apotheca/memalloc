module Memory.IO.Access.Retainable
(
    Retainable(..)
) where

import Memory.IO.Prelude

-- | A retainable counts the number of references to it, allowing it to
-- be deallocated or disposed of once the count reaches 0.
--
-- This is extremely useful in conjunction with bracket mechanisms, eg to allow
-- a resource to safely escape its original scope.
class Retainable r where

    -- | The number of references to the resource.
    retainCount :: r a -> Int

    -- | Increment the reference count.
    retain :: r a -> IO ()

    -- | Decrement the reference count.
    --
    -- If the reference count reaches 0, the resource is to be finalized.
    release :: r a -> IO ()

    -- | Automatically retain and release the resource for the scope of the
    -- provided action.
    --
    -- Part of the class so implementations can override this with eg
    -- exception-handling or interrupt-masking.
    autoreleasing :: r a -> (r a -> IO a) -> IO a
    autoreleasing r f = do
        retain r
        a <- f r
        release r
        pure a

    -- | Set the reference count to an arbitrary value.
    --
    -- This is only exposed for implementation purposes.
    unsafeSetRetainCount :: r a -> Int -> IO ()
    
    -- | Unsafely decrement the reference without checking if it has reached 0.
    --
    -- This is only to be used when you know that the reference count is not 0,
    -- or to allow it to temporarily be 0 when you know you are going to retain
    -- it again in the immediate future.
    --
    -- This is only exposed for implementation purposes.
    unsafeRelease :: r a -> IO ()
