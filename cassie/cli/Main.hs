{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import safe Solve (cassieSolveMain)
import safe Init (cassieInitMain)
import safe System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        "init":name:_ -> cassieInitMain name
        "solve":_ -> cassieSolveMain
        _ -> error "Invalid command. Valid commands include 'init <projectName>' or 'solve'"
    