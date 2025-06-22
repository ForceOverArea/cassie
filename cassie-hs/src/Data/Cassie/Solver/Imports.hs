{-# LANGUAGE Trustworthy #-}
module Data.Cassie.Solver.Imports
    ( buildImportedCtx
    , getCtxFromImport
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Except (runExceptT)
import safe Control.Monad.State (execStateT)
import safe Control.Monad.Trans
import safe Control.Monad.Except (throwError, ExceptT, MonadError)
import safe Data.Cassie.Parser.Lang 
import safe Data.Cassie.Solver.Internal
import System.Directory 
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set

type CassieImport = ExceptT CassieError IO

cassieFileExt :: String
cassieFileExt = ".cas"

buildImportedCtx :: Set.Set String -> ParsedCtx -> Import -> IO (Either CassieError ParsedCtx)
buildImportedCtx parentPaths ctx (importPath, importSet) = runExceptT 
    $ getCtxFromImport parentPaths ctx (importPath, importSet)

-- | Builds a context from parsing imported modules 
getCtxFromImport :: Set.Set String -> ParsedCtx -> Import -> CassieImport ParsedCtx
getCtxFromImport parentPaths ctx (importPath, importSet) = 
    let
        foldChildImports = getCtxFromImport $ Set.insert importPath parentPaths
        needsExporting x _ = x `Set.member` importSet
    in do
        moduleFilePath <- liftIO 
            $ (++ "/" ++ importPath ++ cassieFileExt) 
            <$> getCurrentDirectory
        (childImports, childCtx, childEqns) <- tryBuildModuleCtx moduleFilePath
        checkForRecursion parentPaths childImports
        childProvidedCtx <- foldM foldChildImports childCtx childImports
        (finalCtx, _, _) <- execStateT solveSystemMain (childProvidedCtx, childEqns, Map.empty)
        return . (ctx `Map.union`) $ Map.filterWithKey needsExporting finalCtx

tryBuildModuleCtx :: FilePath -> CassieImport ([Import], ParsedCtx, EquationPool)
tryBuildModuleCtx fp = do
    result <- buildGlobalCtx fp <$> tryGetModuleSource fp
    (throwError ||| return) result

tryGetModuleSource :: FilePath -> CassieImport String
tryGetModuleSource fp = do
    pathIsFake <- liftIO $ not <$> doesFileExist fp
    FileDoesNotExist fp `thrownWhen` pathIsFake
    liftIO $ readFile fp

checkForRecursion :: Set.Set String -> [Import] -> CassieImport ()
checkForRecursion parentPaths childImports = do
    let recursiveImports = parentPaths `Set.intersection` (Set.fromList $ map fst childImports)
    FoundRecursiveImport `thrownWhen` (Set.empty /= recursiveImports)

thrownWhen :: MonadError e f => e -> Bool -> f ()
thrownWhen err = flip when (throwError err)
