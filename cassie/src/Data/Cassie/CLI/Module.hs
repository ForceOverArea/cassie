{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Cassie.CLI.Module
    ( solveModular
    , ParsedCassieError
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Except (runExceptT, throwError, ExceptT)
import safe Control.Monad.RWS (execRWST)
import safe Control.Monad.Writer (runWriterT, tell, WriterT)
import safe Control.Monad.Trans
import safe Data.Cassie.CLI.Module.Internal (CassieModuleError(..))
import safe Data.Cassie.CLI.MonadLookup
import safe Data.Cassie.CLI.Parser.Lang (parseCassiePhrases, Import)
import safe Data.Cassie.CLI.Parser.ParsedTypes
import safe Data.Cassie.Solver.Internal
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set

type CassieModuleT m = ExceptT ParsedCassieError (WriterT [String] m)

type ParsedCassieError = CassieError ParsedMagma ParsedUnary ParsedElement

-- | The file extension used by Cassie source files.
cassieFileExt :: String
cassieFileExt = ".cas"

-- | Given an initial context and a module path (either in the 
--   filesystem provided by @IO@ or a virtual one implementing
--   @MonadLookup@), this function recursively solves a chain of 
--   dependent systems of equations.
solveModular :: ( Monoid (m FilePath)
                , MonadLookup FilePath String m
                ) 
    => FilePath
    -> Symbols
    -> m (Either ParsedCassieError ((ParsedCtx, ParsedSoln)), [String])
solveModular thisModule keySolutions = runWriterT . runExceptT 
    $ solveModularSystem mempty mempty mempty (thisModule, keySolutions)

-- | Builds a context from parsing imported modules.
--   
--   
--   This is effectively @main@ for the CASsie CLI.
solveModularSystem :: ( Monoid (m FilePath)
                      , MonadLookup FilePath String m
                      ) 
    => Set.Set String 
    -> ParsedCtx
    -> ParsedSoln
    -> (String, Symbols) 
    -> CassieModuleT m (ParsedCtx, ParsedSoln)
solveModularSystem dependentModules importedCtx importedSoln (localModule, localExports) = 
    let 
        exports = Map.filterWithKey $ \x _ -> x `Set.member` localExports
        exportUnion x = exports . (x `Map.union`) 
        foldChildImports = uncurry 
            $ solveModularSystem 
            $ localModule `Set.insert` dependentModules
    in do
        moduleFilePath <- lift . lift $ (++ "/" ++ localModule ++ cassieFileExt) <$> pathRoot
        (localDependencies, localCtx, localEqns) <- tryBuildModuleCtx moduleFilePath
        if localDependencies == [] then do
            ((localSoln, unsolved), solnLog) <- execRWST solveConstrainedMain localCtx (Map.empty, localEqns)
            assertConstrained unsolved
            tell solnLog
            return (exports localCtx, exports localSoln)
        else do
            checkForRecursion dependentModules localDependencies
            (importedCtx', importedSoln') <- foldM 
                foldChildImports 
                (importedCtx `Map.union` localCtx, importedSoln)
                localDependencies
            ((localSoln, unsolved), solnLog) <- execRWST solveConstrainedMain importedCtx' (importedSoln', localEqns)
            assertConstrained unsolved
            tell solnLog
            return (importedCtx', importedSoln' `exportUnion` localSoln) -- NOTE: this return statement governs whether child imports vs local symbols ONLY are re-exported 

tryBuildModuleCtx :: MonadLookup FilePath String m 
    => FilePath 
    -> CassieModuleT m ([Import], ParsedCtx, ParsedEqPool)
tryBuildModuleCtx fp 
    = buildGlobalCtx fp 
    <$> tryGetModuleSource fp
    >>= throwError ||| return

-- | Tries to read a source file (or map entry), returning its source
--   if the file exists or throwing a @FileDoesNotExist@ error if it
--   does not.
tryGetModuleSource :: (Monad m, MonadLookup FilePath String m) 
    => FilePath 
    -> CassieModuleT m String
tryGetModuleSource fp 
    = (lift .lift $ lookupM fp) 
    >>= maybe 
        (throwError . ImportError $ FileDoesNotExist fp) 
        return

-- | Builds the global context of a system prior to solving it. This 
--   consists of 3 things:
--
--   1. A list of imported modules that the file (may) depend on
--   2. A @Context@ map structure of symbols to functions or constant values
--   3. A pool of equations parsed out in the system. 
buildGlobalCtx :: FilePath -> String -> Either (CassieError mg u n) ([Import], ParsedCtx, ParsedEqPool)
buildGlobalCtx = curry (left ParserError . uncurry parseCassiePhrases)

-- | Checks a given import for recursive dependencies, 
--   throwing @FoundRecursiveImport@ when a recursive
--   dependency is found.
checkForRecursion :: Monad m 
    => Set.Set String 
    -> [(String, Symbols)] 
    -> CassieModuleT m ()
checkForRecursion parentPaths childImports = 
    let 
        recursiveImports = parentPaths `Set.intersection` (Set.fromList $ map fst childImports)
    in when (Set.empty /= recursiveImports)
        $ throwError . ImportError $ FoundRecursiveImport

-- | Throws a @FailedToContstrain@ error when the given equation pool is not empty.
assertConstrained :: Monad m => ParsedEqPool -> CassieModuleT m ()
assertConstrained x = when ([] /= x) . throwError . FailedToConstrain $ x
