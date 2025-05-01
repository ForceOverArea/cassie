{- |
Module      :  Evaluate
Description :  Monad transformer stack and actions for evaluating algebraic structures
Copyright   :  (c) Grant Christiansen
License     :  MIT

Maintainer  :  christiansengrant18@gmail.com
Stability   :  [unstable] | experimental | provisional | stable | frozen
Portability :  portable

Defines the @Evaluate@ monad transformer stack type and appropriate actions.
This module also exports the @evaluate@ function, which can be used to evaluate
an @AlgebraicStruct@ to a numerical value given enough context.
-}

{-# LANGUAGE Safe #-}
module Data.Cassie.Evaluate
    ( evaluate
    , isConst
    , Context
    , CtxItem(..)
    , EvalError
    ) where

import safe Data.List
import safe qualified Data.Map as Map
import safe Control.Monad.Except (runExcept, throwError, Except)
import safe Control.Monad.Reader (asks, runReaderT, ReaderT)
import safe Control.Monad.Trans (lift)
import safe Data.Cassie.Structures (AlgebraicStruct(..), Symbol)
import safe Data.Cassie.Substitute (substituteFuncArgs, SubstitutionError)

-- | A concrete @Data.Map@ type for providing context to the algorithm
--   for evaluating an @AlgebraicStruct@ to a numeric value.
type Context = Map.Map Symbol CtxItem

-- | A value in a @Context@ instance that may represent either a known
--   constant or a known function in an @AlgebraicStructure@.
data CtxItem 
    = Const AlgebraicStruct 
    | Func [Symbol] AlgebraicStruct
    deriving (Show, Eq)

-- | An error that may be thrown by @evaluate@.
data EvalError 
    = SymbolNotDefined String
    | InvalidArguments Int Int
    | ZeroOrSingleTermPolynomial
    | FailedToCallFunction SubstitutionError
    deriving Eq

instance Show EvalError where
    show (SymbolNotDefined symbol) 
        = "a symbol found in the expression was not defined in the context: '" 
        ++ symbol 
        ++ "'"

    show (InvalidArguments expected actual) 
        = "a function was called without the correct number of arguments (expected " 
        ++ show expected 
        ++ ", actual " 
        ++ show actual
        ++ ")"

    show ZeroOrSingleTermPolynomial
        = "found a structure representing a polynomial expression that had only one term"

    show (FailedToCallFunction err)
        = "failed to evaluate function with arguments: " ++ show err

-- | The @Evaluate@ monad transformer stack for providing immutable access to
--   a @Context@ and reporting errors when attempting to evaluate an @AlgebraicStruct@. 
type Evaluate = ReaderT Context (Except EvalError)

-- | Evaluates a given @AlgebraicStruct@ to a numeric value given some @Context@.
--   This function may fail, returning a @Left EvalError@ in the process.
evaluate :: AlgebraicStruct -> Context -> Either EvalError Double
evaluate expr ctx = runExcept $ runReaderT (evaluateMain expr) ctx

-- | The main control flow for evaluating an @AlgebraicStruct@ to a numeric value.
evaluateMain :: AlgebraicStruct -> Evaluate Double
evaluateMain x = case x of
    Sum terms              -> evaluateSum terms
    Difference subtrahends -> evaluateDiff subtrahends
    Product factors        -> evaluateProd factors
    Quotient d s           -> evaluateQuotient d s
    Exponent b e           -> evaluateExponent b e
    Logarithm b l          -> evaluateLog b l
    Function n a           -> evaluateFunction n a
    Group g                -> evaluateMain g
    Value v                -> return v
    Symbol s               -> getConst s >>= evaluateMain

-- | Control flow for evaluating the sum of multiple terms.
evaluateSum :: [AlgebraicStruct] -> Evaluate Double
evaluateSum terms = sum <$> mapM evaluateMain terms

-- | Control flow for evaluating the difference of multiple subtrahends.
evaluateDiff :: [AlgebraicStruct] -> Evaluate Double
evaluateDiff subtrahends =
    case uncons subtrahends of
        Nothing -> lift $ throwError ZeroOrSingleTermPolynomial
        Just (hd, tl) -> do
            hd' <- evaluateMain hd
            tl' <- mapM evaluateMain tl
            return $ foldl (-) hd' tl'

-- | Control flow for evaluating the product of multiple factors.
evaluateProd :: [AlgebraicStruct] -> Evaluate Double
evaluateProd factors = product <$> mapM evaluateMain factors

-- | Control flow for evaluating the quotient of a dividend and a divisor.
evaluateQuotient :: AlgebraicStruct -> AlgebraicStruct -> Evaluate Double
evaluateQuotient d s = do
    d' <- evaluateMain d
    s' <- evaluateMain s
    return $ d' / s'

-- | Control flow for evaluating the exponent of a base value.
evaluateExponent :: AlgebraicStruct -> AlgebraicStruct -> Evaluate Double
evaluateExponent b e = do
    b' <- evaluateMain b
    e' <- evaluateMain e
    return $ b' ** e'

-- | Control flow for evaluating the logarithm of a base value.
evaluateLog :: AlgebraicStruct -> AlgebraicStruct -> Evaluate Double
evaluateLog b l = do
    b' <- evaluateMain b
    l' <- evaluateMain l
    return $ logBase b' l'

-- | Control flow for evaluating a function with a number of arguments.
evaluateFunction :: String -> [AlgebraicStruct] -> Evaluate Double
evaluateFunction n args = do
    (argNames, func) <- getFunc n
    if length argNames == length args then
        let 
            subbedFunc = substituteFuncArgs func argNames args
        in case subbedFunc of
            Left err -> lift . throwError $ FailedToCallFunction err
            Right result -> evaluateMain result
    else 
        lift . throwError $ InvalidArguments (length argNames) (length args)

-- | Fetches a constant value with the given name from the @Context@.
getConst :: String -> Evaluate AlgebraicStruct
getConst s = do
    cst <- asks (Map.lookup s)
    case cst of
        Just (Const v) -> return v
        _ -> lift . throwError $ SymbolNotDefined s

-- | Fetches a function with the given name from the @Context@. 
getFunc :: String -> Evaluate ([Symbol], AlgebraicStruct)
getFunc s = do
    fn <- asks (Map.lookup s)
    case fn of
        Just (Func c f) -> return (c, f)
        _ -> lift . throwError $ SymbolNotDefined s

-- | Indicates whether a given @CtxItem@ is a @Const@-constructor value.
isConst :: CtxItem -> Bool
isConst (Const _)  = True
isConst (Func _ _) = False
