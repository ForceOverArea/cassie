{-# LANGUAGE Safe #-}
module Data.Cassie.Utils
    ( first'
    , insertAt
    , isNonEmptyLine
    , second'
    , splitStrAt
    , splitStrAt'
    , third
    , throwErr
    , truthTable2
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe qualified Data.List.NonEmpty as NE
import safe Control.Monad.Trans (lift, MonadTrans)
import safe Control.Monad.Except (throwError, ExceptT)
import safe qualified Data.Text as Text

insertAt :: a -> Int -> [a] -> [a]
insertAt x idx xs = (take idx xs) ++ (x:(drop idx xs))

-- | Indicates whether a line of text given as a @String@ contains
--   something other than just whitespace.
isNonEmptyLine :: String -> Bool
isNonEmptyLine str = 
    let 
        p :: Bool -> Char -> Bool
        p b x = b || (not $ x `elem` " \n\r\t")
    in foldl p False str

-- | Splits a string at the given delimiter @Char@.
splitStrAt :: Char -> String -> [String]
splitStrAt c = Prelude.map (Text.unpack . Text.strip) . Text.split (== c) . Text.pack

-- | Splits a string at the given delimiter @Char@, returning a @Data.List.NonEmpty@.
splitStrAt' :: Char -> String -> NE.NonEmpty String
splitStrAt' c = NE.map (Text.unpack . Text.strip) . NE.fromList . Text.split (== c) . Text.pack

throwErr :: (MonadTrans t, Monad m) => e -> t (ExceptT e m) a
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

-- | Similar to @Control.Arrow.first@, but works on a 3-tuple
first' :: (t -> a) -> (t, b, c) -> (a, b, c)
first' f (x, y, z) = (f x, y, z) 

-- | Similar to @Control.Arrow.second@, but works on a 3-tuple
second' :: (t -> b) -> (a, t, c) -> (a, b, c)
second' f (x, y, z) = (x, f y, z) 

-- | Similar to @Control.Arrow.first@ or @Control.Arrow.second@ 
--   but works on the final item in a 3-tuple.
third :: (t -> c) -> (a, b, t) -> (a, b, c)
third f (x, y, z) = (x, y, f z)
