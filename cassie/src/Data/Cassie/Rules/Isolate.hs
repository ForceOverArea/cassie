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
module Data.Cassie.Rules.Isolate 
    ( isolate
    , isolateMagma
    , IsolateError(..)
    , Steps
    ) where

import safe Control.Arrow
import safe Control.Monad.RWS (asks, execRWST, get, gets, put, tell, RWST)
import safe Control.Monad.Except (runExcept, Except)
import safe qualified Data.List.NonEmpty as NE
import safe qualified Data.Map as Map
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Rules.Substitute
import safe Data.Cassie.Structures
import safe Data.Cassie.Utils

type Isolate mg u n = RWST (Symbol, Context mg u n) Steps (Equation mg u n) (Except IsolateError)

-- | A type alias for the log of steps taken while solving a given equation. 
type Steps = [String]

data IsolateError
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

isolate :: AlgebraicStructure mg u n => Context mg u n -> Equation mg u n -> Symbol -> Either IsolateError (Equation mg u n, Steps)
isolate ctx eqn target = runExcept $ execRWST isolateMain (target, ctx) eqn

isolateMain :: AlgebraicStructure mg u n => Isolate mg u n ()
isolateMain = do
    target <- asks fst
    (lhs', rhs') <- gets $ lhs &&& rhs
    logStep
    if target ~? rhs' && not (target ~? lhs') then do
        setLhs rhs'
        modifyRhs (const lhs')
        isolateMain
    else case lhs' of 
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

isolateAdditive :: AlgebraicStructure mg u n => NE.NonEmpty (AlgStruct mg u n) -> Isolate mg u n ()
isolateAdditive terms = do
    wrapperTerms <- Additive <$> isolatePolyTerms terms
    modifyRhs $ \rhs' -> rhs' - wrapperTerms
    isolateMain

isolateMultiplicative :: AlgebraicStructure mg u n => NE.NonEmpty (AlgStruct mg u n) -> Isolate mg u n ()
isolateMultiplicative factors = do
    wrapperTerms <- Multiplicative <$> isolatePolyTerms factors
    modifyRhs (/ wrapperTerms)
    isolateMain

negateRhs :: AlgebraicStructure mg u n => AlgStruct mg u n -> Isolate mg u n ()
negateRhs lhs' = do
    setLhs lhs'
    modifyRhs Negated
    isolateMain

invertRhs :: AlgebraicStructure mg u n => AlgStruct mg u n -> Isolate mg u n ()
invertRhs lhs' = do
    setLhs lhs'
    modifyRhs Inverse
    isolateMain

isolateMagma :: AlgebraicStructure mg u n => mg -> AlgStruct mg u n -> AlgStruct mg u n -> Isolate mg u n ()
isolateMagma op lOperand rOperand = 
    let 
        isolateLeft = do
            setLhs lOperand
            return $ isolateLeftOperand op rOperand

        isolateRight = do
            setLhs rOperand
            return $ isolateRightOperand op lOperand
    in do
        target <- asks fst
        cancelOp <- truthTable2 (target ~?) lOperand rOperand
            (throwErr NeedsPolySolve)
            (throwErr SymbolNotFound)
            (isolateLeft)
            (isolateRight)
        case cancelOp of
            Nothing -> throwErr NonCancellableMagma
            Just f -> modifyRhs f
        isolateMain

isolateUnary :: AlgebraicStructure mg u n => u -> AlgStruct mg u n -> Isolate mg u n ()
isolateUnary op lhs' = do
    setLhs lhs'
    case cancel op of
        Just invOp -> modifyRhs $ Unary invOp
        Nothing -> throwErr NoInverseOp
    isolateMain

isolateN_ary :: AlgebraicStructure mg u n => Symbol -> [AlgStruct mg u n] -> Isolate mg u n ()
isolateN_ary name args = do
    (argNames, expanded) <- getFn name
    if length argNames /= length args then
        throwErr InvalidArguments
    else 
        case substituteFnArgs expanded argNames args of
            Left err -> throwErr $ FunctionCallErr err
            Right x  -> setLhs x
    isolateMain

isolatePolyTerms :: NE.NonEmpty (AlgStruct mg u n) -> Isolate mg u n (NE.NonEmpty (AlgStruct mg u n))
isolatePolyTerms terms = do
    target <- asks fst
    let (wrapped, wrapper) = NE.partition (target ~?) terms
    case wrapped of
        [x] -> setLhs x >> (return $ NE.fromList wrapper)
        []  -> throwErr SymbolNotFound
        _   -> throwErr NeedsPolySolve

modifyRhs :: (AlgStruct mg u n -> AlgStruct mg u n) -> Isolate mg u n ()
modifyRhs f = do
    eqn <- get
    put $ Equation (lhs eqn) (f $ rhs eqn)

setLhs :: AlgStruct mg u n -> Isolate mg u n ()
setLhs lhs' = do
    eqn <- get
    put $ Equation (lhs') (rhs eqn)

getFn :: String -> Isolate mg u n ([Symbol], AlgStruct mg u n)
getFn fnName = do
    ctx <- asks snd
    case fnName `Map.lookup` ctx of
        Just (Func syms expanded _) -> return (syms, expanded)
        Just (Known _ _)            -> throwErr NotAFunction
        Nothing                     -> throwErr FunctionNotDefined

-- | Puts a snapshot of the current state of the equation being solved into the 
--   log of steps taken in the solution. 
logStep :: AlgebraicStructure mg u n => Isolate mg u n ()
logStep = do
    eqn <- get
    let step = showAlgStruct (lhs eqn) ++ " = " ++  showAlgStruct (rhs eqn)
    tell [step]
