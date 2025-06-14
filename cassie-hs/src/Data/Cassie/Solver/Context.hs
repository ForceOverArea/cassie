{-# LANGUAGE Safe #-}
module Data.Cassie.Solver.Context
    ( buildCtxAndEqnPool
    , evaluate'
    , isolate'
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Data.Cassie.Parser.Lang 
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Rules.Isolate
import safe Data.Cassie.Solver.Internal
import safe Data.Cassie.Structures (Equation(..))
import safe Data.Cassie.Structures.Internal (AlgStruct(Nullary, Symbol), Symbol)
import safe Data.List as List
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set

buildCtxAndEqnPool :: String -> Either CassieError (ParsedCtx, EquationPool)
buildCtxAndEqnPool sys = do
    (_imports, eqns, funcs) <- partitionEqnsAndFuncs sys -- TODO: handle imports
    let (consts, trueEqns) = partitionConstsAndEquations eqns
    ctx <- Map.union funcs <$> solveAndEvalConsts consts
    return (ctx, trueEqns)

partitionEqnsAndFuncs :: String -> Either CassieError ([Import], EquationPool, ParsedCtx)
partitionEqnsAndFuncs source = do 
    (eqnPool, ctx, imports) <- left ParseError $ parseCassiePhrases "system" source -- TODO: find way to inject filenames here
    return (imports, eqnPool, ctx)

partitionConstsAndEquations :: EquationPool -> ([(ParsedEqn, Symbol)], EquationPool)
partitionConstsAndEquations = 
    let 
        f2a :: EquationPool -> (EquationPool, EquationPool)
        f2a = partition $ \(eq, syms) -> (Set.size syms == 1) && (isIsolated eq $ Set.findMin syms)

        f2b :: EquationPool -> [(ParsedEqn, Symbol)]
        f2b = map $ second Set.findMin

    in f2a >>> first f2b

solveAndEvalConsts :: [(ParsedEqn, Symbol)] -> Either CassieError ParsedCtx
solveAndEvalConsts xs = 
    let 
        f3 :: [(ParsedEqn, Symbol)] -> Either CassieError [ParsedEqn]
        f3 = mapM $ isolate' Map.empty
            >>> left IsolationError 
            >>> right fst

        f4a :: [ParsedEqn] -> Either CassieError ParsedCtx
        f4a = foldM f4b Map.empty

        f4b :: ParsedCtx -> ParsedEqn -> Either CassieError ParsedCtx
        f4b ctx eqn = do
            (x, result) <- evaluate' Map.empty eqn
            return $ Map.insert x (Const $ Nullary result) ctx

    in f3 xs >>= f4a


isolate' :: ParsedCtx -> (ParsedEqn, Symbol) -> Either IsolateError (ParsedEqn, Steps)
isolate' ctx (eqn, sym) = isolate sym eqn ctx

evaluate' :: ParsedCtx -> ParsedEqn -> Either CassieError (Symbol, Double)
evaluate' ctx (Equation (Symbol x) rhs') = do
    result <- left EvaluationError $ evaluate rhs' ctx
    return (x, result)
evaluate' _ _ = Left EvaluationArgError