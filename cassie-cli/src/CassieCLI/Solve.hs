{-# LANGUAGE Safe #-}
module CassieCLI.Solve 
    ( cassieSolveMain
    , cassieWebMain
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Except
import safe Control.Monad.RWS
import safe Data.Cassie.Solver
import safe Data.List as List
import safe qualified Data.Set as Set
import safe CassieCLI.Module
import safe CassieCLI.MonadVirtIO
import safe CassieCLI.Parser.Lang
import safe CassieCLI.Solve.Formatting
import safe CassieCLI.Solve.Formatting.Web
import safe CassieCLI.Solve.Internal
import safe CassieCLI.Solve.Settings

cassieSolveMain :: (MonadFail m, MonadIO m, MonadVirtIO m) => [String] -> m ()
cassieSolveMain _argv = do
    cassieJSON <- parseCassieJSON <$> vReadFile "./Cassie.json"
    let ep = relPathFile $ entryPoint cassieJSON
    vPutStrLn $ "solving from entry point: " ++ ep
    (_, _, solutionLog) <- runRWST cassieCliMain cassieJSON ()
    vWriteFile (ep ++ ".soln.md") $ intercalate "\n" solutionLog
    pure ()

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
        pure maybeSoln
    let (_ctx, soln) = unwrapEither maybeSoln
    renderSymbolicSolns soln 
    lift $ vSetCurrentDirectory pwd

cassieWebMain :: String -> String
cassieWebMain source = stringifyPossSolution $ do
    (_imports, context, equationPool) <- left (ParserError . show) 
        $ parseCassiePhrases "system" source
    (soln, unsolved) <- solveCassieSystem context mempty equationPool
    when ([] /= unsolved) $ throwError FailedToFullySolve
    pure soln

-- | Retrieves the @Right@-constructed value from an @Either@, calling 
--   @error . show@ on the @Left@-constructed value.
unwrapEither :: Show e => Either e a -> a
unwrapEither = error . show ||| id
