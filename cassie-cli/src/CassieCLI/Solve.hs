{-# LANGUAGE Trustworthy #-}
module CassieCLI.Solve 
    ( cassieSolveMain
    ) where

import safe Control.Arrow
import safe Control.Monad.RWS (asks, runRWST)
import safe Control.Monad.Trans (lift)
import safe CassieCLI.Module (solveModular, relPathDir, relPathFile)
import safe Data.List as List
import safe qualified Data.Set as Set
import safe CassieCLI.Solve.Formatting (renderSymbolicSolns)
import safe CassieCLI.Solve.Internal (CassieCLI)
import safe CassieCLI.MonadVirtFS (MonadVirtFS(..))
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

cassieCliMain :: MonadVirtFS m => CassieCLI m ()
cassieCliMain = do 
    (rootDir, entryModule) <- (relPathDir &&& relPathFile) <$> asks entryPoint
    pwd <- lift $ vGetCurrentDirectory
    lift . vSetCurrentDirectory $ pwd ++ "/" ++ rootDir
    keySolutions <- asks 
        $ solution
        >>> constrained' &&& numeric'
        >>> uncurry (++)
        >>> Set.fromList
    result <- lift $ solveModular entryModule keySolutions
    let (maybeSoln, _showSteps) = (second $ mapM_ putStrLn) result
    let (_ctx, soln) = unwrapEither maybeSoln
    renderSymbolicSolns soln 
    lift $ vSetCurrentDirectory pwd

-- | Retrieves the @Right@-constructed value from an @Either@, calling 
--   @error . show@ on the @Left@-constructed value.
unwrapEither :: Show e => Either e a -> a
unwrapEither = error . show ||| id
