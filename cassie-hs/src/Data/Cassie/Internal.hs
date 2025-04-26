{-# LANGUAGE Safe #-}
module Data.Cassie.Internal 
    ( first'
    , second'
    , third
    , insertAt
    , splitStrAt
    , throwM
    , truthTable2
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Trans (lift, MonadTrans)
import safe Control.Monad.Except (throwError, Except)
import safe qualified Data.Text as Text

-- | Inserts a given value @x@ into a list of values at the @idx@th index.
insertAt :: a -> Int -> [a] -> [a]
insertAt x idx xs = (take idx xs) ++ (x:(drop idx xs))

splitStrAt :: Char -> String -> [String]
splitStrAt c = map (Text.unpack . Text.strip) . Text.split (== c) . Text.pack

-- | Shorthand for throwing an @Except@ monad error
throwM :: MonadTrans t => e -> t (Except e) a
throwM = lift . throwError

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

first' :: (t -> a) -> (t, b, c) -> (a, b, c)
first' f (x, y, z) = (f x, y, z) 

second' :: (t -> b) -> (a, t, c) -> (a, b, c)
second' f (x, y, z) = (x, f y, z) 

third :: (t -> c) -> (a, b, t) -> (a, b, c)
third f (x, y, z) = (x, y, f z)
