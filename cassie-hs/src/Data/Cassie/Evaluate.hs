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

import safe Control.Monad.Except (runExcept, throwError, Except)
import safe Control.Monad.Reader (asks, runReaderT, ReaderT)
import safe Control.Monad.Trans (lift)
import safe Data.Cassie.Structures
import safe Data.Cassie.Substitute (substituteFnArgs, SubstitutionError)
import safe Data.List
import safe qualified Data.Map as Map

type Context m u n = Map.Map String (CtxItem m u n)

-- | An error that may be thrown by @evaluate@.
data EvalError 
    = SymbolNotDefined String
    | InvalidArguments Int Int
    | ZeroOrSingleTermPolynomial
    | FailedToCallFunction SubstitutionError
    deriving Eq

data CtxItem m u n
    = Func [Symbol] (AlgStruct m u n)
    | Const (AlgStruct m u n)
    deriving (Eq, Ord)

instance AlgebraicStructure m u n => Show (CtxItem m u n) where
    show (Const algStruct) =
        case evaluate algStruct Map.empty of
            Left _ -> showAlgStruct algStruct
            Right val -> show val

    show (Func args impl) = "(" ++ intercalate "," args ++ ") -> " ++ showAlgStruct impl

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
type Evaluate m u n = ReaderT (Context m u n) (Except EvalError)

-- | Evaluates a given @AlgebraicStruct@ to a numeric value given some @Context@.
--   This function may fail, returning a @Left EvalError@ in the process.
evaluate :: AlgebraicStructure m u n => AlgStruct m u n -> Context m u n -> Either EvalError n
evaluate expr ctx = runExcept $ runReaderT (evaluateMain expr) ctx

-- | The main control flow for evaluating an @AlgebraicStruct@ to a numeric value.
evaluateMain :: AlgebraicStructure m u n => AlgStruct m u n -> Evaluate m u n n
evaluateMain y = case y of
    Additive ts       -> sum <$> mapM evaluateMain ts
    Multiplicative fs -> product <$> mapM evaluateMain fs
    Negated x         -> negate <$> evaluateMain x
    Inverse x         -> recip <$> evaluateMain x
    Magma op l r      -> do 
        l' <- evaluateMain l 
        r' <- evaluateMain r
        return $ evalMagma op l' r'
    Unary op x        -> do
        x' <- evaluateMain x
        return $ evalUnary op x'
    N_ary n a         -> evaluateFunction n a
    Nullary n         -> return n
    Symbol s          -> getConst s >>= evaluateMain

-- | Control flow for evaluating a function with a number of arguments.
evaluateFunction :: AlgebraicStructure m u n => String -> [AlgStruct m u n] -> Evaluate m u n n
evaluateFunction n args = do
    (argNames, func) <- getFunc n
    if length argNames == length args then
        let 
            subbedFunc = substituteFnArgs func argNames args
        in case subbedFunc of
            Left err -> lift . throwError $ FailedToCallFunction err
            Right result -> evaluateMain result
    else 
        lift . throwError $ InvalidArguments (length argNames) (length args)

-- | Fetches a constant value with the given name from the @Context@.
getConst :: String -> Evaluate m u n (AlgStruct m u n)
getConst s = do
    cst <- asks (Map.lookup s)
    case cst of
        Just (Const v) -> return v
        _ -> lift . throwError $ SymbolNotDefined s

-- | Fetches a function with the given name from the @Context@. 
getFunc :: String -> Evaluate m u n ([Symbol], AlgStruct m u n)
getFunc s = do
    fn <- asks (Map.lookup s)
    case fn of
        Just (Func c f) -> return (c, f)
        _ -> lift . throwError $ SymbolNotDefined s

-- | Indicates whether a given @CtxItem@ is a @Const@-constructor value.
isConst :: CtxItem m u n -> Bool
isConst (Const _)  = True
isConst (Func _ _) = False
