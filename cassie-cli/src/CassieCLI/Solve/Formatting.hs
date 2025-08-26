{-# LANGUAGE Safe #-}
module CassieCLI.Solve.Formatting
    ( renderSymbolicSolns
    ) where

import safe CassieCLI.Solve.Internal (CassieCLI)
import safe CassieCLI.MonadVirtIO (MonadVirtIO(..))
import safe CassieCLI.Parser.ParsedTypes (ParsedSoln)
import safe CassieCLI.Solve.Settings (constrained', numeric', symbolic', CassieJSON(..))
import safe Control.Monad
import safe Control.Monad.RWS (asks, tell)
import safe qualified Data.Cassie.Solver as CSlv (SolutionItem(..))
import safe Data.Cassie.Structures (Symbol)
import safe qualified Data.Map as Map

newLine :: String
newLine = ""

wildCardOnly :: [Symbol]
wildCardOnly = ["*"]

renderSymbolicSolns :: (MonadVirtIO m, MonadFail m) => ParsedSoln -> CassieCLI m ()
renderSymbolicSolns soln = 
    let
        getSolnWhitelist f = keySolutions soln =<< (asks $ f . solution)
    in do
        [showConstrained, showNumeric, showSymbolic] <- mapM getSolnWhitelist [constrained', numeric', symbolic']
        when (showConstrained /= []) $ do 
            logLn "## Constrained Solutions: "
            mapM_ (renderConstrained soln) showConstrained
        when (showNumeric /= []) $ do 
            logLn "## Numeric Solutions: "
            mapM_ (renderNumeric soln) showNumeric
        when (showSymbolic /= []) $ do
            logLn "## Symbolic Solutions: "
            mapM_ (renderSymbolic soln) showSymbolic

renderSymbolic :: MonadVirtIO m => ParsedSoln -> Symbol -> CassieCLI m ()
renderSymbolic soln k = do
    case k `Map.lookup` soln of
        Nothing -> error404 k
        Just (CSlv.SolutionItem symSoln steps _) -> do 
            logLn $ "### " ++ show symSoln
            logLn newLine
            logLn $ tabOut 1 ++ "Steps: "
            tell $ map (tabOut 2 ++) steps
            logLn newLine

renderNumeric :: MonadVirtIO m => ParsedSoln -> Symbol -> CassieCLI m ()
renderNumeric soln k = do
    case k `Map.lookup` soln of
        Nothing -> error404 k
        Just (CSlv.SolutionItem _ _ (Left err))      -> logLn $ "Failed to solve for '" ++ k ++ "': " ++ show err
        Just (CSlv.SolutionItem _ _ (Right numSoln)) -> do 
            logLn $ "### " ++ k ++ " = " ++ show numSoln 
            logLn newLine

renderConstrained :: MonadVirtIO m => ParsedSoln -> Symbol -> CassieCLI m ()
renderConstrained soln k = do
    logLn "TODO: make this section different from numeric solutions"
    case k `Map.lookup` soln of
        Nothing -> error404 k
        Just (CSlv.SolutionItem _ _ (Left err))      -> logLn $ "Failed to solve for '" ++ k ++ "': " ++ show err
        Just (CSlv.SolutionItem _ _ (Right numSoln)) -> do 
            logLn $ "### " ++ k ++ " = " ++ show numSoln 
            logLn newLine

keySolutions :: MonadVirtIO m => ParsedSoln -> [Symbol] -> CassieCLI m [Symbol]
keySolutions soln importList = do
    if wildCardOnly == importList then 
        return $ Map.keys soln
    else 
        return importList

logLn :: MonadVirtIO m => String -> CassieCLI m ()
logLn = tell . pure

error404 :: [Char] -> b
error404 k = error $ "solution for symbol '" ++ k ++ "' could not be found."

tabOut :: Int -> String
tabOut n = concat . take n $ repeat "    "
