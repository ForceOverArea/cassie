{-# LANGUAGE Trustworthy #-}
module CassieCLI.Solve 
    ( cassieSolveMain
    ) where

import safe Control.Arrow
import safe Control.Monad.RWS (asks, runRWST)
import safe Control.Monad.Trans (liftIO)
import safe CassieCLI.Module (solveModular, relPathDir, relPathFile)
import safe Data.List as List
import safe qualified Data.Set as Set
import safe CassieCLI.Solve.Internal (CassieCLI)
import safe CassieCLI.Solve.Formatting (renderSymbolicSolns)
import safe CassieCLI.Solve.Settings (constrained', numeric', parseCassieJSON, CassieJSON(..))
import System.Directory (getCurrentDirectory, setCurrentDirectory)

cassieSolveMain :: [String] -> IO ()
cassieSolveMain _argv = do
    cassieJSON <- parseCassieJSON <$> readFile "./Cassie.json"
    let ep = relPathFile $ entryPoint cassieJSON
    putStrLn $ "solving from entry point: " ++ ep
    (_, _, solutionLog) <- runRWST cassieCliMain cassieJSON ()
    writeFile (ep ++ ".soln.md") $ intercalate "\n" solutionLog
    return ()

cassieCliMain :: CassieCLI ()
cassieCliMain = do 
    (rootDir, entryModule) <- (relPathDir &&& relPathFile) <$> asks entryPoint
    pwd <- liftIO $ getCurrentDirectory
    liftIO . setCurrentDirectory $ pwd ++ "/" ++ rootDir
    keySolutions <- asks 
        $ solution
        >>> constrained' &&& numeric'
        >>> uncurry (++)
        >>> Set.fromList
    result <- liftIO $ solveModular entryModule keySolutions
    let (maybeSoln, _showSteps) = (second $ mapM_ putStrLn) result
    let (_ctx, soln) = unwrapEither maybeSoln
    renderSymbolicSolns soln 
    liftIO $ setCurrentDirectory pwd

-- | Retrieves the @Right@-constructed value from an @Either@, calling 
--   @error . show@ on the @Left@-constructed value.
unwrapEither :: Show e => Either e a -> a
unwrapEither = error . show ||| id
