{-# LANGUAGE Safe #-}
module Data.Cassie.Solver 
    ( buildGlobalCtx
    , buildImportedCtx
    , solvedFor
    , solvedForValue
    , solveSystem
    , showStepsFor
    , CassieError
    , EquationPool
    , Solution
    , SolutionItem(..)
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.RWS (execRWST)
import safe Control.Monad.Except (runExcept)
import safe Data.Cassie.Parser.Lang
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Rules.Isolate
import safe Data.Cassie.Solver.Imports
import safe Data.Cassie.Solver.Internal
import safe Data.Cassie.Structures
import safe qualified Data.Set as Set
import safe qualified Data.Map as Map

solvedFor :: String -> String -> Context mg u n -> Either CassieError (Equation mg u n, Steps)
solvedFor unsolved sym ctx = do
    (structure, syms) <- left ParseError $ parseEquation' unsolved
    when (not $ sym `Set.member` syms) 
        (Left . ConstraintError $ "target symbol did not exist in equation. found symbols: " ++ show syms)
    solution <- left IsolationError $ isolate sym structure ctx
    return solution

solvedForValue :: String -> String -> Context mg u n -> Either CassieError (Double, Equation mg u n, Steps)
solvedForValue unsolved sym ctx = do
    (solvedEqn, steps') <- solvedFor unsolved sym ctx
    value' <- left EvaluationError $ evaluate (rhs solvedEqn) ctx
    return (value', solvedEqn, steps')

solveSystem :: FilePath -> String -> Either CassieError (Context mg u n, Solution)
solveSystem fp sys = do
    (imports, ctx, eqns) <- buildGlobalCtx fp sys
    when ([] /= imports) 
        $ Left ImportsNotAllowed
    (_finalCtx, unsolvedEqns, solnInfo) <- runExcept 
        $ execStateT solveConstrainedMain (ctx, eqns, Map.empty)
    when (length unsolvedEqns > 0)
        $ Left FailedToFullySolve
    return (ctx, solnInfo)
