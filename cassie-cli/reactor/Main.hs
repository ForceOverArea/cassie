{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import safe CassieCLI.Init
import safe CassieCLI.Internal
import safe CassieCLI.Repl
import safe CassieCLI.Solve
import safe Control.Monad.Trans
import safe System.Environment
import safe NodeIO

main :: IO ()
main = error "no main can be provided in a WASI reactor build due to GHC linker behavior"

mainJS :: IO ()
mainJS = runNodeIOT $ 
    lift (drop 1 <$> getArgs) >>= \case
        "init"  : name : argv   -> ensureConfigDirectoryExists consoleLogger >> cassieInitMain name argv
        "repl"  : argv          -> cassieReplMain argv
        "solve" : argv          -> cassieSolveMain argv
        _ -> error "Invalid command. Valid commands include 'init <projectName>' or 'solve'" 

foreign export javascript mainJS :: IO ()
