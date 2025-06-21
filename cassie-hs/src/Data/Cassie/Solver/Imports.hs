{-# LANGUAGE Trustworthy #-}
module Data.Cassie.Solver.Imports
    ( 
    ) where

import safe Control.Monad 
import safe Control.Monad.State (execStateT)
import safe Control.Monad.Trans
import safe Control.Monad.Except (throwError, ExceptT)
import safe Data.Cassie.Parser.Lang 
import safe Data.Cassie.Solver.Context (buildGlobalCtx)
import safe Data.Cassie.Solver.EqSolve (solveSystemMain)
import safe Data.Cassie.Solver.Internal (CassieError(..))
import safe Data.Cassie.Utils
import System.Directory 
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set

type CassieImport = ExceptT CassieError IO

-- | Builds a context from parsing imported modules 
getCtxFromImport :: Set.Set String -> ParsedCtx -> Import -> CassieImport ParsedCtx
getCtxFromImport parentPaths ctx (importPath, importSet) = do
    modulePathInvalid <- liftIO $ not <$> doesFileExist importPath
    when modulePathInvalid $ throwError FileDoesNotExist
    ctxBuildResult <- liftIO $ buildGlobalCtx <$> readFile importPath
    let (imports, childCtx, eqns) = either throwError id ctxBuildResult
    let recursiveImports = parentPaths `Set.intersection` (Set.fromList $ map fst imports)
    when (Set.empty /= recursiveImports) 
        $ throwError FoundRecursiveImport
    when (importSet /= Set.fromList $ Map.keys childCtx)
        $ 
    (_, _, solnInfo) <- execStateT solveSystemMain (ctx, eqns, Map.empty)
    case result of
        Right (finalCtx, _soln) -> return $ finalCtx
        Left err -> throwError err

-- solveSystem :: String -> Either CassieError (ParsedCtx, Solution)
-- solveSystem sys = do
--     (_imports ,ctx, eqns) <- buildGlobalCtx sys
--     (_, _, solnInfo) <- runExcept $ execStateT solveSystemMain (ctx, eqns, Map.empty)
--     return (ctx, solnInfo)