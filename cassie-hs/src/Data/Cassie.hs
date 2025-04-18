{-# LANGUAGE Safe #-}
module Data.Cassie 
    ( solvedFor
    , solvedForValue
    -- , solvedForValue'
    ) where

import Control.Arrow (left)
import Data.Cassie.Evaluate (evaluate, Context, EvalError)
import Data.Cassie.Isolate (isolate, Steps, IsolateError)
import Data.Cassie.Parser (parseEquation, CassieParserError)
import Data.Cassie.Structures (Equation(..))

data CassieError
    = ParseError CassieParserError
    | IsolationError IsolateError
    | EvaluationError EvalError
    deriving Show

solvedFor :: String -> String -> Context -> Either CassieError (Equation, Steps)
solvedFor eqn sym ctx = do
    structure <- left ParseError $ parseEquation eqn
    solution  <- left IsolationError $ isolate structure sym ctx
    return solution

solvedForValue :: String -> String -> Context -> Either CassieError (Double, Equation, Steps)
solvedForValue eqn sym ctx = do
    (eqn', steps) <- solvedFor eqn sym ctx 
    let Equation (_, value) = eqn'
    value' <- left EvaluationError $ evaluate value ctx
    return (value', eqn', steps)
