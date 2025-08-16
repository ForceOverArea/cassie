{-# LANGUAGE Safe #-}
module Data.Cassie 
    ( evaluate
    , isolate
    , solveCassieSystem
    , solveCassieSystemT
    , substitute
    , AlgebraicStructure
    , AlgStruct(..)
    , CassieError(..)
    , Equation(..)
    , EvalError(..)
    , IsolateError(..)
    , SubstitutionError(..)
    , Solution
    , SolutionItem(..)
    ) where

import safe Data.Cassie.Solver (solveCassieSystem, solveCassieSystemT, CassieError(..), Solution, SolutionItem(..))
import safe Data.Cassie.Rules.Evaluate (evaluate, EvalError(..))
import safe Data.Cassie.Rules.Isolate (isolate, IsolateError(..))
import safe Data.Cassie.Rules.Substitute (substitute, SubstitutionError(..))
import safe Data.Cassie.Structures (AlgebraicStructure, AlgStruct(..), Equation(..))