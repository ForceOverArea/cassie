{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cassie.Solver.Internal
    ( Cassie
    , CassieT
    , CassieError(..)
    , EquationPool
    , Solution
    , SolutionItem(..)
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

type Solution = Map.Map Symbol SolutionItem

-- | Similar to @CtxItem@, but contains metadata not needed by the 
--   @evalulate@ function. 
data SolutionItem = SolutionItem
    { eqn     :: ParsedEqn
    , steps   :: Steps
    , possVal :: Either CassieError Double
    }
    deriving (Show, Eq)

type EquationPool = [(ParsedEqn, Symbols)]

data CassieError
    = ConstraintError String
    | EvaluationArgError
    | EvaluationError EvalError
    | FailedToConstrain EquationPool
    | FailedToFullySolve
    | FileDoesNotExist FilePath
    | FoundRecursiveImport
    | IsolationError IsolateError
    | ImportsNotAllowed
    | ImportNotFound (String, [Symbols])
    | ParseError CassieParserError
    deriving (Show, Eq)
