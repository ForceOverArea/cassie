{-# LANGUAGE Safe #-}
module Data.Cassie.Solver 
    ( CassieError
    , EquationPool
    , Solution
    , SolutionItem(..)
    , Symbols
    ) where

import safe Data.Cassie.Solver.Internal
-- TODO: export algorithm that solves a system of equations
--       from an equationpool and initial read-only context
