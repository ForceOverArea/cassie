{-# LANGUAGE Safe #-}
module Data.Cassie.Internal 
    ( parseContextString
    ) where

import safe Control.Arrow ((&&&), second)

parseContextString :: String -> [(String, Double)]
parseContextString ctx = map parseItem items
    where
        items :: [String]
        items = splitStrAt ',' ctx
        
        parseItem :: String -> (String, Double)
        parseItem = second read . get2 . splitStrAt '='

get2 :: [a] -> (a, a)
get2 = head &&& head . tail

splitStrAt :: Char -> String -> [String]
splitStrAt p s = case dropWhile (== p) s of
    "" -> []
    s' -> w : splitStrAt p s''
        where (w, s'') = break (== p) s'