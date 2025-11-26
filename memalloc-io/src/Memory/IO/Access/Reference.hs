module Memory.IO.Access.Reference
(
    Reference(..)
,   MutableReference(..)
) where

import Memory.IO.Prelude

class Reference r where

    dereference :: r a -> IO a

class (Reference r) => MutableReference r where

    setReference :: r a -> a -> IO ()
    updateReference :: r a -> (a -> IO a) -> IO ()
