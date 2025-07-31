{-# LANGUAGE Safe #-}
module Solve 
    ( cassieSolveMain
    ) where

import safe Control.Arrow
import safe Control.Monad.Reader (asks, runReaderT)
import safe Control.Monad.Trans (liftIO)
import safe Data.Cassie.CLI (cassieMain, ParsedSoln)
import safe qualified Data.Cassie.Solver as Slv (SolutionItem(..))
import safe Data.List as List
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set
import safe Solve.Internal (CassieCLI)
import safe Solve.Formatting ()
import safe Solve.Settings (parseCassieJSON, CassieJSON(..), CassieSolnOpts(..))

cassieSolveMain :: [String] -> IO ()
cassieSolveMain _argv = do
    cassieJSON <- parseCassieJSON <$> readFile "./Cassie.json"
    putStrLn $ "solving from entry point: " ++ (show $ entryPoint cassieJSON)
    runReaderT cassieCliMain cassieJSON 

cassieCliMain :: CassieCLI ()
cassieCliMain = do
    ep <- asks entryPoint
    keySolutions <- asks 
        $ constrained . solution &&& numeric . solution
        >>> uncurry (++)
        >>> Set.fromList
    result <- liftIO $ cassieMain ep keySolutions
    let (maybeSoln, _showSteps) = (second $ mapM_ putStrLn) result
    -- liftIO _showSteps
    let (_ctx, soln) = unwrapEither maybeSoln
    -- liftIO $ putStrLn "Context: " >> print _ctx 
    solutionText <- renderSymbolicSolns soln 
    -- liftIO . putStrLn $ solutionText
    liftIO $ writeFile (ep ++ ".soln.md") solutionText

renderSymbolicSolns :: ParsedSoln -> CassieCLI String
renderSymbolicSolns soln = 
    let 
        error404 k = error $ "solution for symbol '" ++ k ++ "' could not be found."

        renderConstrained k = 
            case k `Map.lookup` soln of
                Nothing -> error404 k
                Just (Slv.SolutionItem symSoln steps _) 
                    -> "### " ++ show symSoln ++ "\n\
                    \   Steps: <br>\n"
                    ++ (intercalate " <br>\n" $ map ("       " ++) steps)
                    ++ "<br><br>\n\n"
        
        renderNumeric k = 
            case k `Map.lookup` soln of
                Nothing -> error404 k
                Just (Slv.SolutionItem _ _ (Right numSoln)) 
                    -> "### " ++ k ++ " = " ++ show numSoln
                    ++ "<br><br>\n\n"
                Just (Slv.SolutionItem _ _ (Left err)) 
                    -> "Failed to solve for '" 
                    ++ k 
                    ++ "': " 
                    ++ show err
    in do
        keyConstrainedSolns <- asks $ constrained . solution 
        keyNumericSolns <- asks $ numeric . solution
        return $ "## Numeric Solutions:\n\n"
            ++ concat (map renderNumeric keyNumericSolns)
            ++ "## Constrained Solutions: \n\n"
            ++ concat (map renderConstrained keyConstrainedSolns)

-- | Retrieves the @Right@-constructed value from an @Either@, calling 
--   @error . show@ on the @Left@-constructed value.
unwrapEither :: Show e => Either e a -> a
unwrapEither = error . show ||| id
