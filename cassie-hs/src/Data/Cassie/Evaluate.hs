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

type Context = Map.Map Symbol CtxItem

data CtxItem 
    = Const AlgebraicStruct 
    | Func [Symbol] AlgebraicStruct

data EvalError 
    = SymbolNotDefined String
    | InvalidArguments Int Int
    | ZeroOrSingleTermPolynomial
    | FailedToCallFunction SubstitutionError

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

type Evaluate = ReaderT Context (Except EvalError)

evaluate :: AlgebraicStruct -> Context -> Either EvalError Double
evaluate expr ctx = runExcept $ runReaderT (evaluateMain expr) ctx

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

evaluateSum :: [AlgebraicStruct] -> Evaluate Double
evaluateSum terms = sum <$> mapM evaluateMain terms

evaluateDiff :: [AlgebraicStruct] -> Evaluate Double
evaluateDiff subtrahends =
    case uncons subtrahends of
        Nothing -> lift $ throwError ZeroOrSingleTermPolynomial
        Just (hd, tl) -> do
            hd' <- evaluateMain hd
            tl' <- mapM evaluateMain tl
            return $ foldl (-) hd' tl'

evaluateProd :: [AlgebraicStruct] -> Evaluate Double
evaluateProd factors = product <$> mapM evaluateMain factors

evaluateQuotient :: AlgebraicStruct -> AlgebraicStruct -> Evaluate Double
evaluateQuotient d s = do
    d' <- evaluateMain d
    s' <- evaluateMain s
    return $ d' / s'

evaluateExponent :: AlgebraicStruct -> AlgebraicStruct -> Evaluate Double
evaluateExponent b e = do
    b' <- evaluateMain b
    e' <- evaluateMain e
    return $ b' ** e'

evaluateLog :: AlgebraicStruct -> AlgebraicStruct -> Evaluate Double
evaluateLog b l = do
    b' <- evaluateMain b
    l' <- evaluateMain l
    return $ logBase b' l'

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

getConst :: String -> Evaluate AlgebraicStruct
getConst s = do
    cst <- asks (Map.lookup s)
    case cst of
        Just (Const v) -> return v
        _ -> lift . throwError $ SymbolNotDefined s

getFunc :: String -> Evaluate ([Symbol], AlgebraicStruct)
getFunc s = do
    fn <- asks (Map.lookup s)
    case fn of
        Just (Func c f) -> return (c, f)
        _ -> lift . throwError $ SymbolNotDefined s

isConst :: CtxItem -> Bool
isConst (Const _)  = True
isConst (Func _ _) = False
