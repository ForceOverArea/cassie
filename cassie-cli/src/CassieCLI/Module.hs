{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module CassieCLI.Module
    ( cassieBaseLibrary
    , cassieConfigDir
    , cassieFileExt
    , relPathDir
    , relPathFile
    , solveModular
    , splitStrAt
    , startsWith
    , ParsedCassieError
    ) where

import safe CassieCLI.Module.Internal
import safe CassieCLI.MonadVirtIO
import safe CassieCLI.Parser.Lang (parseCassiePhrases, Import)
import safe CassieCLI.Parser.ParsedTypes
import safe CassieCLI.Utils (splitStrAt, startsWith)
import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Except (runExceptT, throwError, ExceptT)
import safe Control.Monad.RWS (execRWST)
import safe Control.Monad.Writer (runWriterT, tell, WriterT)
import safe Control.Monad.Trans
import safe Data.Cassie.Solver
import safe Data.List as List
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set

type CassieModuleT m = ExceptT ParsedCassieError (WriterT [String] m)

type ParsedCassieError = CassieError ParsedMagma ParsedUnary ParsedElement

-- | Given an initial context and a module path (either in the 
--   filesystem provided by @IO@ or a virtual one implementing
--   @MonadVirtIO@), this function recursively solves a chain of 
--   dependent systems of equations.
solveModular :: MonadVirtIO m 
    => FilePath
    -> Symbols
    -> m (Either ParsedCassieError ((ParsedCtx, ParsedSoln)), [String])
solveModular thisModule keySolutions = runWriterT . runExceptT 
    $ solveModularSystem mempty mempty mempty (thisModule, keySolutions)

-- | Builds a context from parsing imported modules.
--   
--   
--   This is effectively @main@ for the CASsie CLI.
solveModularSystem :: MonadVirtIO m
    => Set.Set String 
    -> ParsedCtx
    -> ParsedSoln
    -> (String, Symbols) 
    -> CassieModuleT m (ParsedCtx, ParsedSoln)
solveModularSystem dependentModules accumCtx accumSoln (localModule, localExports) = 
    let 
        exportWildcard = (Set.size localExports == 1 && Set.findMin localExports == "*")
        exports = Map.filterWithKey $ \x _ -> x `Set.member` localExports || exportWildcard
        exportUnion x = exports . (x `Map.union`) 
        foldChildImports = uncurry 
            $ solveModularSystem 
            $ localModule `Set.insert` dependentModules
    in do
        moduleFilePath <- getModuleOrBaseLibrary localModule
        (localDependencies, localCtx, localEqns) <- tryBuildModuleCtx moduleFilePath
        tell ["local context for " ++ moduleFilePath ++ ": ", show localCtx, "local equations: ", show localEqns]
        if localDependencies == [] then do
            ((localSoln, unsolved), solnLog) <- execRWST solveConstrainedMain localCtx (Map.empty, localEqns)
            assertConstrained unsolved
            tell [ "Exports: ", show (exports localCtx, exports localSoln) ]
            tell $ "Solution: ":solnLog
            return ( (exports localCtx) `Map.union` accumCtx
                   , (exports localSoln) `Map.union` accumSoln
                   )
        else do
            checkForRecursion dependentModules localDependencies
            (importedCtx', importedSoln') <- foldM 
                foldChildImports 
                (localCtx `Map.union` accumCtx, accumSoln) -- TODO: should this union be here? 
                localDependencies
            tell $ [ ""
                   , "imported context post-fold for " ++ moduleFilePath ++ ": ", show importedCtx'
                   , "imported solution post-fold for " ++ moduleFilePath ++ ": ", show importedSoln'
                   ]
            ((localSoln, unsolved), solnLog) <- execRWST solveConstrainedMain importedCtx' (importedSoln', localEqns)
            assertConstrained unsolved
            tell solnLog
            return ( accumCtx `Map.union` exports importedCtx'
                   , accumSoln `Map.union` (importedSoln' `exportUnion` localSoln)
                   ) -- NOTE: this return statement governs whether child imports vs local symbols ONLY are re-exported 

getModuleOrBaseLibrary :: MonadVirtIO m 
    => String 
    -> CassieModuleT m String
getModuleOrBaseLibrary localModule = 
    let
        baseModuleRelPath = intercalate "/" . drop 1 $ splitStrAt '/' localModule
    in lift . lift 
        $ if localModule `startsWith` "Cassie" then
            (++ cassieConfigDir ++ "/" ++ baseModuleRelPath ++ cassieFileExt) <$> vGetHomeDirectory 
        else
            (++ "/" ++ localModule ++ cassieFileExt) <$> vGetCurrentDirectory

tryBuildModuleCtx :: MonadVirtIO m 
    => FilePath 
    -> CassieModuleT m ([Import], ParsedCtx, ParsedEqPool)
tryBuildModuleCtx fp 
    = buildGlobalCtx fp 
    <$> tryGetModuleSource fp
    >>= throwError ||| return

-- | Tries to read a source file (or map entry), returning its source
--   if the file exists or throwing a @FileDoesNotExist@ error if it
--   does not.
tryGetModuleSource :: MonadVirtIO m
    => FilePath 
    -> CassieModuleT m String
tryGetModuleSource fp 
    = (lift .lift $ vTryReadFile fp) 
    >>= maybe 
        (throwError . ImportError . show $ FileDoesNotExist fp) 
        return

-- | Builds the global context of a system prior to solving it. This 
--   consists of 3 things:
--
--   1. A list of imported modules that the file (may) depend on
--   2. A @Context@ map structure of symbols to functions or constant values
--   3. A pool of equations parsed out in the system. 
buildGlobalCtx :: FilePath -> String -> Either (CassieError mg u n) ([Import], ParsedCtx, ParsedEqPool)
buildGlobalCtx = curry (left (ParserError . show) . uncurry parseCassiePhrases)

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
        $ throwError . ImportError . show $ FoundRecursiveImport

-- | Throws a @FailedToConstrain@ error when the given equation pool is not empty.
assertConstrained :: Monad m => ParsedEqPool -> CassieModuleT m ()
assertConstrained x = when ([] /= x) . throwError . FailedToConstrain $ x
