{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Cassie.CLI.Utils
    ( isNonEmptyLine
    , splitStrAt
    , splitStrAt'
    , startsWith
    ) where

import safe qualified Data.List.NonEmpty as NE
import safe qualified Data.Text as Text

-- | Indicates whether a line of text given as a @String@ contains
--   something other than just whitespace.
isNonEmptyLine :: String -> Bool
isNonEmptyLine str = 
    let 
        p :: Bool -> Char -> Bool
        p b x = b || (not $ x `elem` [' ', '\n', '\r', '\t'])
    in foldl p False str

-- | Splits a string at the given delimiter @Char@.
splitStrAt :: Char -> String -> [String]
splitStrAt c = Prelude.map (Text.unpack . Text.strip) . Text.split (== c) . Text.pack

-- | Splits a string at the given delimiter @Char@, returning a @Data.List.NonEmpty@.
splitStrAt' :: Char -> String -> NE.NonEmpty String
splitStrAt' c = NE.map (Text.unpack . Text.strip) . NE.fromList . Text.split (== c) . Text.pack

startsWith :: String -> String -> Bool
startsWith word = (`Text.isPrefixOf` Text.pack word) . Text.pack
