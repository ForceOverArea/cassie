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
    ( isIsolated
    , isolate
    , isolateMagma
    , IsolateError
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

type Isolate m u n = RWST (Symbol, Context m u n) Steps (Equation m u n) (Except IsolateError)

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

isolate :: AlgebraicStructure m u n => Symbol -> Equation m u n -> Context m u n -> Either IsolateError (Equation m u n, Steps)
isolate target eqn ctx = runExcept $ execRWST isolateMain (target, ctx) eqn

isolateMain :: AlgebraicStructure m u n => Isolate m u n ()
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

isolateAdditive :: AlgebraicStructure m u n => NE.NonEmpty (AlgStruct m u n) -> Isolate m u n ()
isolateAdditive terms = do
    wrapperTerms <- Additive <$> isolatePolyTerms terms
    modifyRhs $ \rhs' -> rhs' - wrapperTerms
    isolateMain

isolateMultiplicative :: AlgebraicStructure m u n => NE.NonEmpty (AlgStruct m u n) -> Isolate m u n ()
isolateMultiplicative factors = do
    wrapperTerms <- Multiplicative <$> isolatePolyTerms factors
    modifyRhs (/ wrapperTerms)
    isolateMain

negateRhs :: AlgebraicStructure m u n => AlgStruct m u n -> Isolate m u n ()
negateRhs lhs' = do
    setLhs lhs'
    modifyRhs Negated
    isolateMain

invertRhs :: AlgebraicStructure m u n => AlgStruct m u n -> Isolate m u n ()
invertRhs lhs' = do
    setLhs lhs'
    modifyRhs Inverse
    isolateMain

isolateMagma :: AlgebraicStructure m u n => m -> AlgStruct m u n -> AlgStruct m u n -> Isolate m u n ()
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

isolateUnary :: AlgebraicStructure m u n => u -> AlgStruct m u n -> Isolate m u n ()
isolateUnary op lhs' = do
    setLhs lhs'
    case cancel op of
        Just invOp -> modifyRhs $ Unary invOp
        Nothing -> throwErr NoInverseOp
    isolateMain

isolateN_ary :: AlgebraicStructure m u n => Symbol -> [AlgStruct m u n] -> Isolate m u n ()
isolateN_ary name args = do
    (argNames, expanded) <- getFn name
    if length argNames /= length args then
        throwErr InvalidArguments
    else 
        case substituteFnArgs expanded argNames args of
            Left err -> throwErr $ FunctionCallErr err
            Right x  -> setLhs x
    isolateMain

isolatePolyTerms :: NE.NonEmpty (AlgStruct m u n) -> Isolate m u n (NE.NonEmpty (AlgStruct m u n))
isolatePolyTerms terms = do
    target <- asks fst
    let (wrapped, wrapper) = NE.partition (target ~?) terms
    case wrapped of
        [x] -> setLhs x >> (return $ NE.fromList wrapper)
        []  -> throwErr SymbolNotFound
        _   -> throwErr NeedsPolySolve

modifyRhs :: (AlgStruct m u n -> AlgStruct m u n) -> Isolate m u n ()
modifyRhs f = do
    eqn <- get
    put $ Equation (lhs eqn) (f $ rhs eqn)

setLhs :: AlgStruct m u n -> Isolate m u n ()
setLhs lhs' = do
    eqn <- get
    put $ Equation (lhs') (rhs eqn)

getFn :: String -> Isolate m u n ([Symbol], AlgStruct m u n)
getFn fnName = do
    ctx <- asks snd
    case fnName `Map.lookup` ctx of
        Just (Func syms expanded _) -> return (syms, expanded)
        Just (Known _ _)            -> throwErr NotAFunction
        Nothing                     -> throwErr FunctionNotDefined

isIsolated :: AlgebraicStructure m u n => Equation m u n -> Symbol -> Bool
isIsolated eqn sym = case isolate sym eqn Map.empty of
    Left _ -> False
    Right (eqn', _) -> eqn == eqn'

-- | Puts a snapshot of the current state of the equation being solved into the 
--   log of steps taken in the solution. 
logStep :: AlgebraicStructure m u n => Isolate m u n ()
logStep = do
    eqn <- get
    let step = showAlgStruct (lhs eqn) ++ " = " ++  showAlgStruct (rhs eqn)
    tell [step]
