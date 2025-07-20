{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Cassie.CLI.Module
    ( checkForRecursion
    , tryGetModuleSource
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Except (runExceptT)
import safe Control.Monad.Reader (asks ,ReaderT)
import safe Control.Monad.RWS (execRWST)
import safe Control.Monad.Trans
import safe Control.Monad.Except (throwError, ExceptT, MonadError)
import safe Data.Cassie.CLI.MonadLookup
import safe Data.Cassie.CLI.Module.Internal (CassieModuleError(..))
import safe Data.Cassie.CLI.Parser.Internal (CassieParserError(..))
import safe Data.Cassie.CLI.Parser.Lang (parseCassiePhrases, Import)
import safe Data.Cassie.CLI.Parser.Lexemes (Symbols)
import safe Data.Cassie.CLI.Parser.ParsedTypes
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Solver.Internal
import safe qualified Data.Map as Map
import safe Data.Maybe
import safe qualified Data.Set as Set
import System.Directory
import safe Data.Cassie.Utils

type CassieModuleT = ExceptT (CassieError ParsedMagma ParsedUnary ParsedElement)

-- | The file extension used by Cassie source files.
cassieFileExt :: String
cassieFileExt = ".cas"

-- | Checks a given import for recursive dependencies, 
--   throwing @FoundRecursiveImport@ when a recursive
--   dependency is found.
checkForRecursion :: Monad m => Set.Set String -> [(String, Symbols)] -> CassieModuleT m ()
checkForRecursion parentPaths childImports = 
    let 
        recursiveImports = parentPaths `Set.intersection` (Set.fromList $ map fst childImports)
    in FoundRecursiveImport `thrownWhen` (Set.empty /= recursiveImports)

-- buildImportedCtx :: MonadLookup FilePath String m
--     => Set.Set String 
--     -> (Context mg u n) 
--     -> (String, Symbols) 
--     -> m (Either (CassieError mg u n) (Context mg u n))
-- buildImportedCtx parentPaths ctx (importPath, importSet) = runExceptT 
--     $ getCtxFromImport parentPaths ctx (importPath, importSet)

-- | Builds a context from parsing imported modules 
getCtxFromImport :: MonadLookup FilePath String m
    => Set.Set String 
    -> ParsedCtx
    -> (String, Symbols) 
    -> CassieModuleT m (ParsedCtx)
getCtxFromImport parentPaths ctx (importPath, importSet) = 
    let
        foldChildImports = getCtxFromImport $ importPath `Set.insert` parentPaths
        needsExporting x _ = x `Set.member` importSet
    in do
        moduleFilePath <- liftIO 
            $ (++ "/" ++ importPath ++ cassieFileExt) 
            <$> getCurrentDirectory
        (childImports, childCtx, childEqns) <- tryBuildModuleCtx moduleFilePath
        checkForRecursion parentPaths childImports
        childProvidedCtx <- foldM foldChildImports childCtx childImports
        ((finalCtx, remainingEquations), _) <- execRWST solveConstrainedMain childProvidedCtx (Map.empty, childEqns)
        if [] /= remainingEquations then
            throwError . FailedToConstrain $ remainingEquations
        else
            -- TODO: need to merge input CTX and derived solution
            return . (ctx `Map.union`) $ Map.filterWithKey needsExporting finalCtx

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
    = (lift $ lookupM fp) 
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

-- | similar to @Control.Monad.when@, but calls @Control.Monad.Except.throwError@ 
--   on the left argument.
thrownWhen :: MonadError e f => e -> Bool -> f ()
thrownWhen err = flip when (throwError err)
