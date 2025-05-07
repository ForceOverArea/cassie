module Cassie.Structures.Internal
    ( Equation(..)
    , Invertible(..)
    ) where

-- | A class for types of structures that can 
class Invertible s where
    (^-) :: s a -> (a -> Bool) -> (a, a -> t a)

    inversion :: s a -> (a -> Bool) -> a -> t a
    inversion x = snd . (x ^-)

    unwrap :: s a -> (a -> Bool) -> a
    unwrap x = fst . (x ^-)

newtype Equation a = Equation (a, a)
