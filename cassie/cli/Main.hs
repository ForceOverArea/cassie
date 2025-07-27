{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import safe Control.Arrow
import safe Control.Monad.Reader (asks, runReaderT)
import safe Control.Monad.Trans (liftIO)
import safe Data.Cassie.CLI (cassieMain, ParsedSoln)
import safe Data.List as List
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set
import safe Internal (CassieCLI)
import safe Formatting ()
import safe Settings (parseCassieJSON, CassieJSON(..), CassieSolnOpts(..))

main :: IO ()
main = do
    cassieJSON <- parseCassieJSON <$> readFile "./Cassie.json"
    putStrLn $ "solving from entry point: '" ++ (show $ entryPoint cassieJSON) ++ "'"
    runReaderT cassieCliMain cassieJSON 

cassieCliMain :: CassieCLI ()
cassieCliMain = do
    ep <- asks entryPoint
    keySolutions <- asks 
        $ constrained . solution &&& numeric . solution
        >>> uncurry (++)
        >>> Set.fromList
    result <- liftIO $ cassieMain ep keySolutions
    let ((_ctx, soln), _showSteps) = (unwrapEither *** mapM_ putStrLn) result
    -- liftIO showSteps
    liftIO $ putStrLn "Context: " >> print _ctx 
    solutionText <- renderSymbolicSolns soln 
    liftIO . putStrLn $ solutionText
    liftIO $ writeFile (ep ++ ".soln.txt") solutionText

renderSymbolicSolns :: ParsedSoln -> CassieCLI String
renderSymbolicSolns soln = 
    let 
        renderSoln k v = k ++ show v
    in do
        return . intercalate "\n" . Map.elems 
            $ Map.mapWithKey renderSoln soln

-- | Retrieves the @Right@-constructed value from an @Either@, calling 
--   @error . show@ on the @Left@-constructed value.
unwrapEither :: Show e => Either e a -> a
unwrapEither = error . show ||| id
