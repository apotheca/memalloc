module Memory.IO.Array
(
    Array(..)
,   MutableArray(..)
) where

import Memory.IO.Prelude

class Array arr where

    -- type family Idx arr

    length :: arr a -> Int
    index :: arr a -> Int -> a

class (Array arr) => MutableArray arr where

    setIndex :: arr a -> Int -> a -> IO ()
    updateIndex :: arr a -> Int -> (a -> IO a) -> IO ()
