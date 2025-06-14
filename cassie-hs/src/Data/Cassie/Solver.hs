module Data.Cassie.Solver 
    ( solvedFor
    , solvedForValue
    , solveSystem
    ) where

import Data.Cassie.Parser.Internal
import Data.Cassie.Parser.Lang

solvedFor :: String -> String -> ParsedCtx -> Either CassieError (ParsedEqn, Steps)
solvedFor eqn sym ctx = do
    (structure, syms) <- left ParseError $ parseEquation' eqn
    when (not $ sym `Set.member` syms) 
        (Left . ConstraintError $ "target symbol did not exist in equation. found symbols: " ++ show syms)
    solution <- left IsolationError $ isolate sym structure ctx
    return solution

solvedForValue :: String -> String -> ParsedCtx -> Either CassieError (Double, ParsedEqn, Steps)
solvedForValue eqn sym ctx = do
    (eqn', steps) <- solvedFor eqn sym ctx
    value' <- left EvaluationError $ evaluate (rhs eqn') ctx
    return (value', eqn', steps)

solveSystem :: String -> Either CassieError (ParsedCtx, Solution)
solveSystem sys = do
    (ctx, eqns) <- buildCtxAndEqnPool sys
    (_, _, solnInfo) <- runExcept $ execStateT solveSystemMain (ctx, eqns, Map.empty)
    return (ctx, solnInfo)