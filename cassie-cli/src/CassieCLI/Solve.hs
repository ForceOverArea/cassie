{-# LANGUAGE Safe #-}
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
import safe CassieCLI.MonadVirtIO (MonadVirtIO(..))
import safe CassieCLI.Solve.Settings (constrained', numeric', parseCassieJSON, CassieJSON(..))
import safe Control.Monad.IO.Class (MonadIO(..))

cassieSolveMain :: (MonadFail m, MonadIO m, MonadVirtIO m) => [String] -> m ()
cassieSolveMain _argv = do
    cassieJSON <- parseCassieJSON <$> vReadFile "./Cassie.json"
    let ep = relPathFile $ entryPoint cassieJSON
    vPutStrLn $ "solving from entry point: " ++ ep
    (_, _, solutionLog) <- runRWST cassieCliMain cassieJSON ()
    vWriteFile (ep ++ ".soln.md") $ intercalate "\n" solutionLog
    return ()

cassieCliMain :: (MonadFail m, MonadVirtIO m) => CassieCLI m ()
cassieCliMain = do 
    (rootDir, entryModule) <- (relPathDir &&& relPathFile) <$> asks entryPoint
    pwd <- lift $ vGetCurrentDirectory
    lift . vSetCurrentDirectory $ pwd ++ "/" ++ rootDir
    keySolutions <- asks 
        $ solution
        >>> constrained' &&& numeric'
        >>> uncurry (++)
        >>> Set.fromList
    maybeSoln <- lift $ do
        (maybeSoln, showSteps) <- solveModular entryModule keySolutions
        mapM_ vPutStrLn showSteps
        return maybeSoln
    let (_ctx, soln) = unwrapEither maybeSoln
    renderSymbolicSolns soln 
    lift $ vSetCurrentDirectory pwd

-- | Retrieves the @Right@-constructed value from an @Either@, calling 
--   @error . show@ on the @Left@-constructed value.
unwrapEither :: Show e => Either e a -> a
unwrapEither = error . show ||| id
