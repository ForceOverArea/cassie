{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import safe Solve (cassieSolveMain)
import safe Init (cassieInitMain)
import safe Internal (ensureConfigDirectoryExists, consoleLogger)
import safe System.Environment (getArgs)

main :: IO ()
main = let 
        head' = take 1
        tail' = drop 1
    in do
        ensureConfigDirectoryExists consoleLogger
        args <- getArgs
        case head' args of
            "init":name:_ -> cassieInitMain name $ tail' args
            "solve":_ -> cassieSolveMain $ tail' args
            _ -> error "Invalid command. Valid commands include 'init <projectName>' or 'solve'"
    