{-# LANGUAGE Safe #-}
module Solve 
    ( cassieSolveMain
    ) where

import safe Control.Arrow
import safe Control.Monad.RWS (asks, runRWST)
import safe Control.Monad.Trans (liftIO)
import safe Data.Cassie.CLI (cassieMain)
import safe Data.List as List
import safe qualified Data.Set as Set
import safe Solve.Internal (CassieCLI)
import safe Solve.Formatting (renderSymbolicSolns)
import safe Solve.Settings (constrained', numeric', parseCassieJSON, CassieJSON(..))

cassieSolveMain :: [String] -> IO ()
cassieSolveMain _argv = do
    cassieJSON <- parseCassieJSON <$> readFile "./Cassie.json"
    let ep = entryPoint cassieJSON
    putStrLn $ "solving from entry point: " ++ ep
    (_, _, solutionLog) <- runRWST cassieCliMain cassieJSON ()
    writeFile (ep ++ ".soln.md") $ intercalate "\n" solutionLog
    return ()

cassieCliMain :: CassieCLI ()
cassieCliMain = do
    ep <- asks entryPoint
    keySolutions <- asks 
        $ solution
        >>> constrained' &&& numeric'
        >>> uncurry (++)
        >>> Set.fromList
    result <- liftIO $ cassieMain ep keySolutions
    let (maybeSoln, _showSteps) = (second $ mapM_ putStrLn) result
    let (_ctx, soln) = unwrapEither maybeSoln
    renderSymbolicSolns soln 

-- | Retrieves the @Right@-constructed value from an @Either@, calling 
--   @error . show@ on the @Left@-constructed value.
unwrapEither :: Show e => Either e a -> a
unwrapEither = error . show ||| id
