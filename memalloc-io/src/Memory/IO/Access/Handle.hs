module Memory.IO.Access.Handle
(
    Handle(..)
) where

import Memory.IO.Prelude

class Handle h where

    handleEqual :: h a -> h a -> Bool
    handleHash :: h a -> Int
