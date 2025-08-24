{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import safe CassieCLI.Init (cassieInitMain)
import safe CassieCLI.Internal (ensureConfigDirectoryExists, consoleLogger)
import safe CassieCLI.Repl (cassieReplMain)
import safe CassieCLI.Solve (cassieSolveMain)
import safe Control.Monad.Trans (lift)
import safe System.Environment (getArgs)
import safe NodeIO (NodeIOT(runNodeIOT))

main :: IO ()
main = error "no main can be provided in a WASI reactor build"

mainJS :: IO ()
mainJS = runNodeIOT $ 
    lift (drop 1 <$> getArgs) >>= \case
        "init"  : name : argv   -> ensureConfigDirectoryExists consoleLogger >> cassieInitMain name argv
        "repl"  : argv          -> cassieReplMain argv
        "solve" : argv          -> cassieSolveMain argv
        _ -> error "Invalid command. Valid commands include 'init <projectName>' or 'solve'" 

foreign export javascript mainJS :: IO ()
