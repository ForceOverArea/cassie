{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import safe Solve (cassieSolveMain)
import safe Init (cassieInitMain)
import safe Internal (ensureConfigDirectoryExists, consoleLogger)
import safe System.Environment (getArgs)

main :: IO () -- TODO: fix argv processing
main = do
    ensureConfigDirectoryExists consoleLogger
    args <- getArgs
    case args of
        "init"  : name : argv   -> cassieInitMain name argv
        "solve" : argv          -> cassieSolveMain argv
        _ -> error "Invalid command. Valid commands include 'init <projectName>' or 'solve'"
    