{-# LANGUAGE Safe #-}
module Data.Cassie.Internal 
    ( insertAt
    , truthTable2
    ) where

import Control.Arrow ((***))
import Control.Monad (join)

-- | Inserts a given value @x@ into a list of values at the @idx@th index.
insertAt :: a -> Int -> [a] -> [a]
insertAt x idx xs = (take idx xs) ++ (x:(drop idx xs))

-- | @truthTable2 p x y q1 q2 q3 q4@
--
--   Represents a 2-input truth table whose inputs are the predicate @p@
--   mapped over the values @x@ and @y@. 
--
--   The output is chosen according to the following logic:
--
--   p x, p y
--
--   T, T -> @q1@  
--   F, F -> @q2@  
--   T, F -> @q3@  
--   F, T -> @q4@  
truthTable2 :: (a -> Bool) -> a -> a -> b -> b -> b -> b -> b
truthTable2 p x y q1 q2 q3 q4 
    = case join (***) p (x, y) of
        (True, True) -> q1
        (False, False) -> q2
        (True, False) -> q3
        (False, True) -> q4