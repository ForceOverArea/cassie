{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}
module Repl 
    ( cassieReplMain
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.State (evalStateT, get, modify, put, StateT)
import safe Control.Monad.Trans (liftIO)
import safe Data.Cassie.CLI 
import safe Data.Cassie.Solver
import safe qualified Data.Map as Map
import safe System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

type CassieRepl = StateT CassieReplState IO

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

cassieReplMain :: [String] -> IO ()
cassieReplMain _argv = do
    hSetBuffering stdout NoBuffering
    evalStateT cassieRepl mempty
    return ()

cassieRepl :: CassieRepl ()
cassieRepl = do
    _ <- liftIO $ putStr cursor 
    command <- liftIO getLine
    case command of 
        "quit" -> return ()
        "show" -> solved <$> get 
            >>= showSolution
            >> cassieRepl
        lexeme -> do
            missingSemi <- case parsePhrase lexeme of 
                Right (ParsedImport (path, syms)) 
                    -> importSource path syms
                    >> return False
                Right parsedItem
                    -> processReplState parsedItem
                    >> return False
                Left _err -- If we failed to parse a phrase, then try again after concatenating a semicolon
                    -> return True 
            when missingSemi 
                $ case parsePhrase $ lexeme ++ ";" of
                    Right parsedItem 
                        -> processReplState parsedItem
                    Left err 
                        -> liftIO $ print err
            cassieRepl -- Loop again until user quits

importSource :: FilePath -> Symbols -> CassieRepl ()
importSource fp imports = do
    result <- liftIO $ fst <$> cassieMain fp imports 
    case result of  
        Left err -> liftIO $ print err
        Right (ctx, soln) -> do
            modify . context' $ Map.union ctx
            modify . solved' $ Map.union soln

processReplState :: Phrase -> CassieRepl ()
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

showSolnWhenSolvingEquation :: CassieRepl ()
showSolnWhenSolvingEquation = 
    do
        CassieReplState ctx soln eqPool <- get
        updatedSolution <- liftIO $ solveCassieSystemT ctx soln eqPool
        case updatedSolution of
            Left err -> liftIO $ print err
            Right (soln', eqPool') -> do
                let newSoln = soln' `Map.difference` soln
                if newSoln /= Map.empty then do
                    showSolution newSoln
                    put (CassieReplState ctx soln' eqPool')
                else 
                    liftIO $ putStrLn "equation not solvable... yet"
                return ()

showSolution :: ParsedSoln -> CassieRepl ()
showSolution solutionMap = 
    let 
        renderNumericSolnForRepl = either (const "") show . possVal
        renderUsefulParts = 
            constrained &&& renderNumericSolnForRepl
            >>> \(x, y) -> show x ++ "    (" ++ y ++ ")"
    in liftIO $ mapM_ (putStrLn . renderUsefulParts) solutionMap

context' :: (ParsedCtx -> ParsedCtx) -> CassieReplState -> CassieReplState
context' f (CassieReplState a b c) = CassieReplState (f a) b c

solved' :: (ParsedSoln -> ParsedSoln) -> CassieReplState -> CassieReplState
solved' f (CassieReplState a b c) = CassieReplState a (f b) c

unsolved' :: (ParsedEqPool -> ParsedEqPool) -> CassieReplState -> CassieReplState
unsolved' f (CassieReplState a b c) = CassieReplState a b (f c)
