{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Except (runExcept)
import safe Control.Monad.Reader (asks, runReaderT, ReaderT)
import safe Control.Monad.State (execStateT)
import safe Control.Monad.Trans (liftIO)
import safe Data.Cassie.Parser (parseCassiePhrases)
import safe Data.Cassie.Solver (buildImportedCtx, solveSystemMain)
import safe qualified Data.Map as Map 
import safe qualified Data.Set as Set 
import safe qualified Data.Text as Text
import safe Settings (parseCassieJSON, CassieJSON(..))

type CassieCLI = ReaderT CassieJSON IO 

main :: IO ()
main = do
    cassieJSON <- parseCassieJSON <$> readFile "./Cassie.json"
    putStrLn $ "entry point: " ++ (show $ entryPoint cassieJSON)
    source <- readFile $ entryPoint cassieJSON 
    putStrLn $ "source: " ++ source
    runReaderT cassieCliMain cassieJSON 

cassieCliMain :: CassieCLI ()
cassieCliMain = do
    cassieSourceFilepath <- asks entryPoint
    cassieSource <- liftIO $ readFile cassieSourceFilepath
    let (imports, fnCtx, equations) = crashOnErr
            $ parseCassiePhrases cassieSourceFilepath cassieSource
    let foldImportedCtxs = (buildImportedCtx Set.empty) . crashOnErr
    importedCtx <- liftIO $ crashOnErr <$> foldM foldImportedCtxs (pure fnCtx) imports
    let possSolution = crashOnErr . runExcept 
            $ execStateT solveSystemMain (importedCtx, equations, Map.empty)
    liftIO $ writeFile (getSolnFilePath cassieSourceFilepath) $ show possSolution
    return ()

getSolnFilePath :: String -> String
getSolnFilePath = Text.unpack . Text.replace ".cas" ".txt" . Text.pack 

crashOnErr :: Show e => Either e a -> a
crashOnErr = error . show ||| id