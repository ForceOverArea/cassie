{-# LANGUAGE Safe #-}
module Data.Cassie.Solver.Internal
    ( Cassie
    , CassieError(..)
    , EquationPool
    , Solution
    , SolutionValues
    ) where

import safe Control.Monad.Except (ExceptT)
import safe Control.Monad.Identity (Identity)
import safe Control.Monad.State (StateT)
import safe qualified Data.Map as Map
import safe Data.Cassie.Parser.Internal (CassieParserError, Symbols)
import safe Data.Cassie.Parser.Lang (ParsedCtx, ParsedEqn)
import safe Data.Cassie.Rules.Evaluate 
import safe Data.Cassie.Rules.Isolate
import safe Data.Cassie.Structures.Internal (Symbol)

-- | The Cassie 'compiler' monad for statefully building a 
--   solution to a system of equations.
type Cassie m u n = CassieT m u n Identity

type CassieT mg u n m = StateT (ParsedCtx, EquationPool, Solution) (ExceptT CassieError m)

type Solution = Map.Map Symbol SolutionValues

type SolutionValues = (ParsedEqn, Steps, Either CassieError Double)

type EquationPool = [(ParsedEqn, Symbols)]

data CassieError
    = ParseError CassieParserError
    | IsolationError IsolateError
    | EvaluationError EvalError
    | EvaluationArgError
    | ConstraintError String
    | FailedToFullySolve
    | FileDoesNotExist 
    | FoundRecursiveImport
    | FailedToConstrain EquationPool
    deriving (Show, Eq)