module Memory.IO.Access.Retainable
(
    Retainable(..)
) where

import Memory.IO.Prelude

class Retainable r where

    retainCount :: r a -> Int

    retain :: r a -> IO ()
    release :: r a -> IO ()

    unsafeSetRetainCount :: r a -> Int -> IO ()
    unsafeRelease :: r a -> IO ()
