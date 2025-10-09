{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}
module CassieCLI.Repl 
    ( cassieReplMain
    ) where

import safe CassieCLI.Module (solveModular)
import safe CassieCLI.MonadVirtIO (MonadVirtIO(..))
import safe CassieCLI.Parser.ParsedTypes 
import safe CassieCLI.Parser.Lang (parsePhrase, Phrase(..))
import safe Control.Arrow
import safe Control.Monad.IO.Class (MonadIO(..))
import safe Control.Monad.State (evalStateT, get, lift, modify, put, StateT)
import safe Data.Cassie.Solver 
import safe qualified Data.Map as Map
import safe System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

type CassieRepl m = StateT CassieReplState m 

data CassieReplState = CassieReplState
    { context  :: ParsedCtx 
    , solved   :: ParsedSoln
    , unsolved :: ParsedEqPool
    }
    deriving (Show, Eq)

instance Semigroup CassieReplState where
    x <> y = (context' . Map.union $ context x)
        . (solved' . Map.union $ solved x)
        . (unsolved' . (++) $ unsolved x) $ y

instance Monoid CassieReplState where
    mempty = CassieReplState mempty mempty mempty

    mconcat = foldl (<>) mempty

cursor :: String
cursor = ">>> "

cassieReplMain :: (MonadVirtIO m, MonadIO m) => [String] -> m ()
cassieReplMain _argv = do
    liftIO $ hSetBuffering stdout NoBuffering
    vPutStrLn "Welcome to the CASsie v0.1.0.0 REPL!"
    vPutStrLn "Enter \'help\' for more info or \'quit\' to leave."
    evalStateT cassieRepl mempty
    return ()

cassieRepl :: (MonadVirtIO m, MonadIO m) => CassieRepl m ()
cassieRepl = 
    let
        -- | Adds a semicolon to the lexeme, reparses it, and 
        --   vPrints the given error if the reparse fails.
        addSemicolonAndReparse lexeme err = 
            case parsePhrase $ lexeme ++ ";" of
                Right parsedItem -> processReplState parsedItem
                Left _err -> liftIO $ vPrint err
        
        -- | Adds a semicolon and reparses the given lexeme for convenience
        catchMissingSemicolonError = 
            maybe (pure ()) . addSemicolonAndReparse
    in do
    _ <- liftIO $ putStr cursor 
    command <- lift $ vGetLine
    case command of 
        "quit" -> return ()
        "show" -> do
            solved <$> get >>= showSolution
            cassieRepl
        lexeme -> do
            parseError <- case parsePhrase lexeme of 
                Right (ParsedImport (path, syms)) 
                    -> importSource path syms
                    >> return Nothing
                Right parsedItem
                    -> processReplState parsedItem
                    >> return Nothing
                Left err 
                    -> return $ Just err
            catchMissingSemicolonError lexeme parseError
            cassieRepl

importSource :: (MonadVirtIO m, MonadIO m) => FilePath -> Symbols -> CassieRepl m ()
importSource fp imports = do
    result <- liftIO $ fst <$> solveModular fp imports 
    case result of
        Left err -> liftIO $ vPrint err
        Right (ctx, soln) -> do
            modify . context' $ Map.union ctx
            modify . solved' $ Map.union soln

processReplState :: (MonadVirtIO m, MonadIO m) => Phrase -> CassieRepl m ()
processReplState phrs = do
    case phrs of
        ParsedConst (sym, item) -> modify . context' $ Map.insert sym item
        ParsedFn (fnName, item) -> modify . context' $ Map.insert fnName item
        ParsedEqn eqPoolEntry   -> do 
            modify . unsolved' $ (eqPoolEntry :)
            showSolnWhenSolvingEquation
        -- Do not handle import statements as they should be handled by a separate branch of control flow
        _                       -> return ()
    return ()

showSolnWhenSolvingEquation :: (MonadVirtIO m, MonadIO m) => CassieRepl m ()
showSolnWhenSolvingEquation = 
    do
        CassieReplState ctx soln eqPool <- get
        updatedSolution <- solveCassieSystemT ctx soln eqPool
        case updatedSolution of
            Left err -> liftIO $ vPrint err
            Right (soln', eqPool') -> do
                let newSoln = soln' `Map.difference` soln
                if newSoln /= Map.empty then do
                    showSolution newSoln
                    put (CassieReplState ctx soln' eqPool')
                else 
                    liftIO $ vPutStrLn "equation not solvable... yet"
                return ()

showSolution :: (MonadVirtIO m, MonadIO m) => ParsedSoln -> CassieRepl m ()
showSolution solutionMap = 
    let 
        renderNumericSolnForRepl = either (const "") show . possVal
        renderUsefulParts = 
            constrained &&& renderNumericSolnForRepl
            >>> \(x, y) -> show x ++ "    (" ++ y ++ ")"
    in liftIO $ mapM_ (vPutStrLn . renderUsefulParts) solutionMap

context' :: (ParsedCtx -> ParsedCtx) -> CassieReplState -> CassieReplState
context' f (CassieReplState a b c) = CassieReplState (f a) b c

solved' :: (ParsedSoln -> ParsedSoln) -> CassieReplState -> CassieReplState
solved' f (CassieReplState a b c) = CassieReplState a (f b) c

unsolved' :: (ParsedEqPool -> ParsedEqPool) -> CassieReplState -> CassieReplState
unsolved' f (CassieReplState a b c) = CassieReplState a b (f c)
