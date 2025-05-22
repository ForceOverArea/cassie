{-# LANGUAGE Safe #-}
module Data.Cassie.Utils
    ( insertAt
    , splitStrAt
    , throwErr
    , truthTable2
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Trans (lift, MonadTrans)
import safe Control.Monad.Except (throwError, Except)
import safe qualified Data.Text as Text

insertAt :: a -> Int -> [a] -> [a]
insertAt x idx xs = (take idx xs) ++ (x:(drop idx xs))

-- | Splits a string at the given delimiter @Char@.
splitStrAt :: Char -> String -> [String]
splitStrAt c = Prelude.map (Text.unpack . Text.strip) . Text.split (== c) . Text.pack

throwErr :: MonadTrans t => e -> t (Except e) a
throwErr = lift . throwError 

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
