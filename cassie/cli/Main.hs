{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Except (runExcept)
import safe Control.Monad.Reader (asks, runReaderT)
import safe Control.Monad.State (execStateT)
import safe Control.Monad.Trans (liftIO)
import safe Data.Cassie.Parser
import safe Data.Cassie.Solver
import safe qualified Data.Map as Map 
import safe qualified Data.Set as Set 
import safe qualified Data.Text as Text
import safe Internal (CassieCLI)
import safe Settings (parseCassieJSON, CassieJSON(..))

main :: IO ()
main = do
    cassieJSON <- parseCassieJSON <$> readFile "./Cassie.json"
    putStrLn $ "solving from entry point: '" ++ (show $ entryPoint cassieJSON) ++ "'"
    runReaderT cassieCliMain cassieJSON 

cassieCliMain :: CassieCLI ()
cassieCliMain = do
        (imports, fnCtx, equations) <- parseModule
        importedCtx <- addImportsToCtx imports fnCtx
        let possSolution = unwrapEither . runExcept 
                $ execStateT solveSystemMain 
                (importedCtx, equations, Map.empty)
        outputFilepath <- getSolnFilepath
        liftIO 
            $ writeFile outputFilepath
            $ show possSolution

parseModule :: CassieCLI ([Import], ParsedCtx, EquationPool)
parseModule = do
    cassieSourceFilepath <- asks entryPoint
    cassieSource <- liftIO $ readFile cassieSourceFilepath
    either 
        (error . show)
        return
        $ parseCassiePhrases cassieSourceFilepath cassieSource 

addImportsToCtx :: [Import] -> ParsedCtx -> CassieCLI ParsedCtx
addImportsToCtx imports fnCtx = 
    let
        foldImportedCtxs = buildImportedCtx Set.empty . unwrapEither
    in liftIO $ unwrapEither <$> foldM foldImportedCtxs (pure fnCtx) imports

getSolnFilepath :: CassieCLI String
getSolnFilepath = Text.unpack 
    . Text.replace ".cas" ".txt" 
    . Text.pack 
    <$> asks entryPoint

-- | Retrieves the @Right@-constructed value from an @Either@, calling 
--   @error . show@ on the @Left@-constructed value.
unwrapEither :: Show e => Either e a -> a
unwrapEither = error . show ||| id