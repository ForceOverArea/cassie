{-# LANGUAGE Safe #-}
module Data.Cassie 
    ( solvedFor
    , solvedForValue
    -- , solveSystem
    -- , solveSystemNumerically
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Data.List
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set
import safe Control.Monad.State (runStateT, StateT)
import safe Control.Monad.Except (runExcept, Except)
import safe Data.Cassie.Internal
import safe Data.Cassie.Evaluate (evaluate, Context, CtxItem(..), EvalError)
import safe Data.Cassie.Isolate (isolate, Steps, IsolateError)
import safe Data.Cassie.Parser (parseEquation, CassieParserError)
import safe Data.Cassie.Parser.Lang (CassieLangError)
import safe Data.Cassie.Structures (AlgebraicStruct(..), Equation(..), Symbol)

-- | The Cassie 'compiler' monad for statefully building a 
--   solution to a system of equations.
type Cassie = StateT (Context, EquationPool) (Except CassieError)

type Symbols = Set.Set Symbol

type EquationPool = [(AlgebraicStruct, Symbols)]

data CassieError
    = ParseError CassieParserError
    | FunctionParseError CassieLangError
    | IsolationError IsolateError
    | EvaluationError EvalError
    | ConstraintError String
    deriving Show

solvedFor :: String -> String -> Context -> Either CassieError (Equation, Steps)
solvedFor eqn sym ctx = do
    (structure, syms) <- left ParseError $ parseEquation eqn
    when (not $ sym `Set.member` syms) 
        (Left . ConstraintError $ "target symbol did not exist in equation. found symbols: " ++ show syms)
    solution  <- left IsolationError $ isolate structure sym ctx
    return solution

solvedForValue :: String -> String -> Context -> Either CassieError (Double, Equation, Steps)
solvedForValue eqn sym ctx = do
    (eqn', steps) <- solvedFor eqn sym ctx 
    let Equation (_, value) = eqn'
    value' <- left EvaluationError $ evaluate value ctx
    return (value', eqn', steps)

-- solveSystem :: String -> Either CassieError (Map.Map Symbol AlgebraicStruct)
-- solveSystem sys = 

-- solveSystemNumerically :: String -> Either CassieError (Map.Map Symbol Double)
-- solveSystemNumerically sys = Right Map.empty


partitionEqnsAndFuncs :: String -> (Either CassieError [(Equation, Symbols)], [String])
partitionEqnsAndFuncs = 
    let 
        f1a ::String -> ([String], [String])
        f1a = partition ('=' `elem`) . splitStrAt '\n'

        f1b :: [String] -> Either CassieError [(Equation, Symbols)]
        f1b = mapM $ left ParseError . parseEquation

    in f1a >>> first f1b

partitionConstsAndEquations :: [(Equation, Symbols)] -> ([(Equation, Symbol)], [(Equation, Symbols)])
partitionConstsAndEquations = 
    let 
        f2a :: [(Equation, Symbols)] -> ([(Equation, Symbols)], [(Equation, Symbols)])
        f2a = partition $ (== 1) . Set.size . snd

        f2b :: [(Equation, Symbols)] -> [(Equation, Symbol)]
        f2b = map $ second Set.findMin

    in f2a >>> first f2b

solveAndEvalConsts :: [(Equation, Symbol)] -> Either CassieError Context
solveAndEvalConsts = 
    let 
        f3a :: [(Equation, Symbol)] -> Either CassieError [Equation]
        f3a = mapM
            $ (flip $ uncurry isolate) Map.empty
            >>> left IsolationError
            >>> right fst

        f3b :: [Equation] -> Either CassieError Context
        f3b = foldM f4 Map.empty

        -- All equations known to be pre-solved, ok to implement as partial function here:
        f4 :: Context -> Equation -> Either CassieError Context
        f4 ctx (Equation (Symbol x, rhs)) = do
            result <- left EvaluationError $ evaluate rhs Map.empty
            return $ Map.insert x (Const $ Value result) ctx
        f4 _ _ = error "not reachable"
        
    in f3a >>> right f3b