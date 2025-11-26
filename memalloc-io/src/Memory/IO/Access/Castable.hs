module Memory.IO.Access.Castable
(
    Castable(..)
) where

import Memory.IO.Prelude

-- TODO: Castable vs Reinterpretable eg reinterpret_cast
class Castable r where

    cast :: r a -> r b
