module Numeric.Matrix 
    (
    ) where

data Matrix a = 
    Matrix { rows :: a 
           , cols :: a 
           , flatten :: [a]
           } 
           deriving (Eq, Ord)