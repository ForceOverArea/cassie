{-# LANGUAGE Safe #-}
module Data.Cassie.Evaluate
    ( evaluate
    , Context
    , CtxItem(..)
    ) where

import safe Control.Monad.Except (runExcept, throwError, Except)
import safe Control.Monad.Reader (asks, runReaderT, ReaderT)
import safe Control.Monad.Trans (lift)
import safe Data.Cassie.Structures (AlgebraicStruct(..), Symbol)
import safe Data.List (uncons)
import safe qualified Data.Map as M (lookup, Map)

type Context = M.Map String CtxItem

data CtxItem 
    = Const AlgebraicStruct 
    | Func [Symbol] AlgebraicStruct

data EvalError 
    = SymbolNotDefined String
    | InvalidArguments Int Int
    | ZeroOrSingleTermPolynomial

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

type Evaluate = ReaderT Context (Except EvalError)

evaluate :: AlgebraicStruct -> Context -> Either EvalError Double
evaluate expr ctx = runExcept $ runReaderT (evaluate' expr) ctx

evaluate' :: AlgebraicStruct -> Evaluate Double
evaluate' (Sum terms) = sum <$> mapM evaluate' terms

evaluate' (Difference subtrahends) =
    case uncons subtrahends of
        Nothing -> lift $ throwError ZeroOrSingleTermPolynomial
        Just (hd, tl) -> do
            hd' <- evaluate' hd
            tl' <- mapM evaluate' tl
            return $ foldl (-) hd' tl'

evaluate' (Product factors) = product <$> mapM evaluate' factors

evaluate' (Quotient d s) = do
    d' <- evaluate' d
    s' <- evaluate' s
    return $ d' / s'

evaluate' (Exponent b e) = do
    b' <- evaluate' b
    e' <- evaluate' e
    return $ b' ** e'

evaluate' (Logarithm b l) = do
    b' <- evaluate' b
    l' <- evaluate' l
    return $ logBase b' l'

evaluate' (Function n a) = do
    numericArgs <- mapM evaluate' a
    (argc, func) <- getFunc n
    if argc == length a then
        return $ func numericArgs
    else 
        lift . throwError $ InvalidArguments argc (length a)
    
evaluate' (Group g) = evaluate' g

evaluate' (Value v) = return v

evaluate' (Symbol s) = getConst s

getConst :: String -> Evaluate Double
getConst s = do
    cst <- asks (M.lookup s)
    case cst of
        Just (Const v) -> return v
        _ -> lift . throwError $ SymbolNotDefined s

getFunc :: String -> Evaluate (Int, [Double] -> Double)
getFunc s = do
    fn <- asks (M.lookup s)
    case fn of
        Just (Func c f) -> return (c, f)
        _ -> lift . throwError $ SymbolNotDefined s