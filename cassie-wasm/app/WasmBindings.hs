{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Cassie
-- import qualified Data.Map as Map
import Glue (cassieWrapper)
import GHC.Wasm.Prim

main :: IO ()
main = error "not necessary"

-- | Exports the Cassie solver as a function.
solveSystemHs :: JSString -> JSString
solveSystemHs = toJSString . cassieWrapper . solveSystem . fromJSString

-- | Exports Cassie's symbolic isolation algorithm.
-- solveEqnForHs :: JSString -> JSString -> JSString
-- solveEqnForHs eqn tgt = 
--     let
--         result = solvedFor (fromJSString eqn) (fromJSString tgt) Map.empty 
--     in case result of
--         Right (eqn, steps) -> show eqn

foreign export javascript solveSystemHs :: JSString -> JSString
