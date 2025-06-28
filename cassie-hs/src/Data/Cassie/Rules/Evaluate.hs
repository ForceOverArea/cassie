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
module Data.Cassie.Rules.Evaluate
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
import safe Data.Cassie.Rules.Substitute (substituteFnArgs, SubstitutionError)
import safe Data.List
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set

type Context mg u n = Map.Map String (CtxItem mg u n)

-- | An error that may be thrown by @evaluate@.
data EvalError 
    = SymbolNotDefined String
    | InvalidArguments Int Int
    | ZeroOrSingleTermPolynomial
    | FailedToCallFunction SubstitutionError
    deriving Eq

data CtxItem mg u n
    = Func  { arguments      :: [Symbol]
            , implementation :: AlgStruct mg u n
            , dependencies   :: Set.Set Symbol
            }
    | Known { numeric        :: n
            }
    deriving (Eq, Ord)

instance AlgebraicStructure mg u n => Show (CtxItem mg u n) where
    show (Known val) = show val

    show (Func args impl _) = "(" ++ intercalate "," args ++ ") -> " ++ showAlgStruct impl

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
type Evaluate mg u n = ReaderT (Context mg u n) (Except EvalError)

-- | Evaluates a given @AlgebraicStruct@ to a numeric value given some @Context@.
--   This function may fail, returning a @Left EvalError@ in the process.
evaluate :: AlgebraicStructure mg u n => Context mg u n -> AlgStruct mg u n -> Either EvalError n
evaluate ctx expr = runExcept $ runReaderT (evaluateMain expr) ctx

-- | The main control flow for evaluating an @AlgebraicStruct@ to a numeric value.
evaluateMain :: AlgebraicStructure mg u n => AlgStruct mg u n -> Evaluate mg u n n
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
    Symbol s          -> Nullary <$> getConst s >>= evaluateMain

-- | Control flow for evaluating a function with a number of arguments.
evaluateFunction :: AlgebraicStructure mg u n => String -> [AlgStruct mg u n] -> Evaluate mg u n n
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
getConst :: String -> Evaluate mg u n n
getConst s = do
    cst <- asks (Map.lookup s)
    case cst of
        Just (Known v) -> return v
        _ -> lift . throwError $ SymbolNotDefined s

-- | Fetches a function with the given name from the @Context@. 
getFunc :: String -> Evaluate mg u n ([Symbol], AlgStruct mg u n)
getFunc s = do
    fn <- asks (Map.lookup s)
    case fn of
        Just (Func c f _) -> return (c, f)
        _ -> lift . throwError $ SymbolNotDefined s

-- | Indicates whether a given @CtxItem@ is a @Known@-constructor value.
isConst :: CtxItem mg u n -> Bool
isConst (Known _)  = True
isConst (Func _ _ _) = False
