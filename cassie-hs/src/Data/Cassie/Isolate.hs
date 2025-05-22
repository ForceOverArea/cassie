{- |
Module      :  Isolate
Description :  Monad transformer stack and actions for isolating some symbol in an algebraic structure
Copyright   :  (c) Grant Christiansen
License     :  MIT

Maintainer  :  christiansengrant18@gmail.com
Stability   :  [unstable] | experimental | provisional | stable | frozen
Portability :  portable

Defines the @Isolate@ monad transformer stack type and appropriate actions.
This module also exports the @isolate@ function, which can be used to symbolically
solve equations given as an @AlgebraicStruct@.
-}

{-# LANGUAGE Safe #-}
module Data.Cassie.Isolate 
    ( isolate
    ) where

import safe Control.Arrow
import safe Control.Monad.RWS (asks, gets, modify, execRWST, RWST)
import safe Control.Monad.Except (runExcept, Except)
import safe qualified Data.List.NonEmpty as NE
import safe qualified Data.Map as Map
import safe Data.Cassie.Structures.Magmas (isolateLeftOperand, isolateRightOperand, CancelMagma(..))
import safe Data.Cassie.Structures.UnarySystems (CancelUnary(..))
import safe Data.Cassie.Structures.Internal
import safe Data.Cassie.Substitute
import safe Data.Cassie.Utils

type Isolate m u n = RWST (Symbol, Context m u n) String (Equation m u n) (Except IsolateErr)

data IsolateErr
    = FunctionCallErr SubstitutionError
    | FunctionNotDefined
    | InvalidArguments
    | IsolatedConstantSomehow
    | NeedsPolySolve
    | NoInverseOp
    | NonCancellableMagma
    | NotAFunction
    | SymbolNotFound
    | WrongSymbolSomehow
    deriving (Show, Eq, Ord)

isolate :: (CancelMagma m, CancelUnary u, AlgElement n) => Symbol -> Equation m u n -> Context m u n -> Either IsolateErr (Equation m u n)
isolate target eqn ctx = fst <$> (runExcept $ execRWST isolateMain (target, ctx) eqn )

isolateMain :: (CancelMagma m, CancelUnary u, AlgElement n) => Isolate m u n ()
isolateMain = do
    target <- asks fst
    lhs <- gets fst
    case lhs of 
        Additive terms          -> isolateAdditive terms
        Multiplicative factors  -> isolateMultiplicative factors
        Negated neg             -> negateRhs neg
        Inverse inv             -> invertRhs inv
        Magma magma l r         -> isolateMagma magma l r
        Unary unary x           -> isolateUnary unary x
        N_ary name args         -> isolateN_ary name args
        Nullary _               -> throwErr IsolatedConstantSomehow
        Symbol s                
            -> if s == target then 
                return () 
            else 
                throwErr WrongSymbolSomehow

isolateAdditive :: (CancelMagma m, CancelUnary u, AlgElement n) => NE.NonEmpty (AlgStruct m u n) -> Isolate m u n ()
isolateAdditive terms = do
    wrapperTerms <- Additive <$> isolatePolyTerms terms
    modifyRhs $ \rhs -> rhs - wrapperTerms
    isolateMain

isolateMultiplicative :: (CancelMagma m, CancelUnary u, AlgElement n) => NE.NonEmpty (AlgStruct m u n) -> Isolate m u n ()
isolateMultiplicative factors = do
    wrapperTerms <- Multiplicative <$> isolatePolyTerms factors
    modifyRhs (/ wrapperTerms)
    isolateMain

negateRhs :: (CancelMagma m, CancelUnary u, AlgElement n) => AlgStruct m u n -> Isolate m u n ()
negateRhs lhs = do
    setLhs lhs
    modifyRhs Negated
    isolateMain

invertRhs :: (CancelMagma m, CancelUnary u, AlgElement n) => AlgStruct m u n -> Isolate m u n ()
invertRhs lhs = do
    setLhs lhs
    modifyRhs Inverse
    isolateMain

isolateMagma :: (CancelMagma m, CancelUnary u, Fractional n) => m -> AlgStruct m u n -> AlgStruct m u n -> Isolate m u n ()
isolateMagma op lOperand rOperand = do
    target <- asks fst
    cancelOp <- truthTable2 (target ~?) lOperand rOperand
        (throwErr NeedsPolySolve)
        (throwErr SymbolNotFound)
        (pure $ isolateLeftOperand  op lOperand)
        (pure $ isolateRightOperand op rOperand)
    case cancelOp of
        Just f -> modifyRhs f
        Nothing -> throwErr NonCancellableMagma

isolateUnary :: (CancelMagma m, CancelUnary u, AlgElement n) => u -> AlgStruct m u n -> Isolate m u n ()
isolateUnary op lhs = do
    setLhs lhs
    case cancel op of
        Just invOp -> modifyRhs $ Unary invOp
        Nothing -> throwErr NoInverseOp
    isolateMain

isolateN_ary :: (Eq m, Eq u, Eq n) => Symbol -> [AlgStruct m u n] -> Isolate m u n ()
isolateN_ary name args = do
    (argNames, expanded) <- getFn name
    if length argNames /= length args then
        throwErr InvalidArguments
    else 
        case substituteFnArgs expanded argNames args of
            Left err -> throwErr $ FunctionCallErr err
            Right x  -> setLhs x
 
isolatePolyTerms :: NE.NonEmpty (AlgStruct m u n) -> Isolate m u n (NE.NonEmpty (AlgStruct m u n))
isolatePolyTerms terms = do
    target <- asks fst
    let (wrapped, wrapper) = NE.partition (target ~?) terms
    case wrapped of
        [x] -> setLhs x >> (return $ NE.fromList wrapper)
        []  -> throwErr SymbolNotFound
        _   -> throwErr NeedsPolySolve

modifyRhs :: (AlgStruct m u n -> AlgStruct m u n) -> Isolate m u n ()
modifyRhs f = modify $ second f

setLhs :: AlgStruct m u n -> Isolate m u n ()
setLhs lhs = modify (first $ const lhs)

getFn :: String -> Isolate m u n ([Symbol], AlgStruct m u n)
getFn fnName = do
    ctx <- asks snd
    case fnName `Map.lookup` ctx of
        Just (Func syms expanded) -> return (syms, expanded)
        Just (Const _)            -> throwErr NotAFunction
        Nothing                   -> throwErr FunctionNotDefined
