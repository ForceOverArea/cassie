{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Cassie (solveSystem)
import Glue (cassieWrapper)
import GHC.Wasm.Prim

main :: IO ()
main = error "not necessary"

solveSystemHs :: JSString -> JSString
solveSystemHs = toJSString . cassieWrapper . solveSystem . fromJSString

foreign export javascript solveSystemHs :: JSString -> JSString
