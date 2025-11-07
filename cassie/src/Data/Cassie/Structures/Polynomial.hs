module Data.Cassie.Structures.Polynomial 
    ( Polynomial(..)
    ) where

import Data.Cassie.Structures.Internal (Symbol)

class Polynomial a where

    -- | Indicates the degree of a polynomial if it is a polynomial at all.
    --   If the given structure is not considered a polynomial, then 
    degree :: Symbol -> a -> Maybe Int
