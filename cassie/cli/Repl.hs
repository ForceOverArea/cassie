{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE LambdaCase #-}
module Repl 
    ( cassieReplMain
    ) where

import safe Control.Monad.State (evalStateT, modify, StateT)
import safe Control.Monad.Trans (liftIO)
import safe Data.Cassie.CLI 
import safe Data.Cassie.Solver
import safe qualified Data.Map as Map
import System.Directory (getCurrentDirectory)

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
    evalStateT cassieRepl mempty
    return ()

cassieRepl :: CassieRepl ()
cassieRepl = do
    command <- liftIO 
        $ putStr cursor 
        >> getLine
    case command of 
        "quit" -> return ()
        other -> case parsePhrase other of 
            Right (ParsedImport (path, syms)) 
                -> importSource path syms
            Right parsedItem 
                -> processReplState parsedItem
            Left err 
                -> liftIO $ print err
    cassieRepl -- Loop again until user quits

importSource :: FilePath -> Symbols -> CassieRepl ()
importSource fp imports = do
    pwd <- liftIO getCurrentDirectory
    result <- liftIO $ fst <$> cassieMain (pwd ++ "/" ++ fp) imports 
    case result of  
        Left err -> liftIO $ print err
        Right (ctx, soln) -> do
            modify . context' $ Map.union ctx
            modify . solved' $ Map.union soln

processReplState :: Phrase -> CassieRepl ()
processReplState phrs = do
    case phrs of
        ParsedConst (sym, item) -> modify . context'  $ Map.insert sym item
        ParsedFn (fnName, item) -> modify . context'  $ Map.insert fnName item
        ParsedEqn eqPoolEntry   -> modify . unsolved' $ (eqPoolEntry :)
        -- Do not handle import statements as they should be handled by a separate branch of control flow
        _                       -> return ()

context' :: (ParsedCtx -> ParsedCtx) -> CassieReplState -> CassieReplState
context' f (CassieReplState a b c) = CassieReplState (f a) b c

solved' :: (ParsedSoln -> ParsedSoln) -> CassieReplState -> CassieReplState
solved' f (CassieReplState a b c) = CassieReplState a (f b) c

unsolved' :: (ParsedEqPool -> ParsedEqPool) -> CassieReplState -> CassieReplState
unsolved' f (CassieReplState a b c) = CassieReplState a b (f c)
