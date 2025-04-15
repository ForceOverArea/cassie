{-# LANGUAGE Safe #-}
module Data.Cassie 
    ( solvedFor
    , solvedForValue
    -- , solvedForValue'
    ) where

import Control.Arrow (left, second)
import Data.Cassie.Evaluate (evaluate, Context, CtxItem(..))
import Data.Cassie.Isolate (isolate, Steps)
import Data.Cassie.Parser (parseEquation)
import Data.Cassie.Structures (Equation(..))
import Data.Map (fromList)

solvedFor :: String -> String -> Either String (Equation, Steps)
solvedFor eqn sym = do
    result <- (`isolate` sym) <$> parseEquation eqn
    stringizeError result

solvedForValue :: String -> String -> Context -> Either String (Double, Equation, Steps)
solvedForValue eqn sym ctx = do
    (eqn', steps) <- eqn `solvedFor` sym
    let Equation (_, value) = eqn'
    value' <- stringizeError $ evaluate value ctx
    return (value', eqn', steps)

-- solvedForValue' :: String -> String -> [(String, Double)] -> Either String (Double, Equation, Steps)
-- solvedForValue' eqn sym ctx = solvedForValue eqn sym ctx'
--     where 
--         ctx' = fromList (map (second Const) ctx)

stringizeError :: Show a => Either a b -> Either String b
stringizeError = left show