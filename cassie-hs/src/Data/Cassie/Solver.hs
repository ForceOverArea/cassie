{-# LANGUAGE Safe #-}
module Data.Cassie.Solver 
    ( solvedFor
    , solvedForValue
    , solveSystem
    , showStepsFor
    , CassieError
    , Solution
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.State (execStateT)
import safe Control.Monad.Except (runExcept)
import safe Data.Cassie.Parser.Lang
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Rules.Isolate
import safe Data.Cassie.Solver.Context
import safe Data.Cassie.Solver.EqSolve
import safe Data.Cassie.Solver.Internal
import safe Data.Cassie.Structures
import safe qualified Data.Set as Set
import safe qualified Data.Map as Map

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
    (_imports ,ctx, eqns) <- buildGlobalCtx sys
    (finalCtx, unsolvedEqns, solnInfo) <- runExcept 
        $ execStateT solveSystemMain (ctx, eqns, Map.empty)
    if length unsolvedEqns > 0 then
        return $ Left 
    else 
        return (ctx, solnInfo)