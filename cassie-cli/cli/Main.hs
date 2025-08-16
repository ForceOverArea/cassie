{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import safe CassieCLI.Init (cassieInitMain)
import safe CassieCLI.Internal (ensureConfigDirectoryExists, consoleLogger)
import safe CassieCLI.Repl (cassieReplMain)
import safe CassieCLI.Solve (cassieSolveMain)
import safe System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case
    "init"  : name : argv   -> ensureConfigDirectoryExists consoleLogger >> cassieInitMain name argv
    "repl"  : argv          -> cassieReplMain argv
    "solve" : argv          -> cassieSolveMain argv
    _ -> error "Invalid command. Valid commands include 'init <projectName>' or 'solve'"
