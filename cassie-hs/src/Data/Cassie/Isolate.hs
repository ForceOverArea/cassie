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
    , Steps
    ) where

import safe Prelude hiding (exp, log)
import safe Data.List
import safe Control.Monad
import safe qualified Data.Map as Map
import safe Control.Monad.Trans (lift)
import safe Control.Monad.RWS (asks, get, put, tell, execRWST, RWST(..))
import safe Control.Monad.Except (throwError, runExcept, Except)
import safe Data.Cassie.Evaluate (Context, CtxItem(..))
import safe Data.Cassie.Internal (truthTable2)
import safe Data.Cassie.Structures ((~?), AlgebraicStruct(..), Equation (Equation), Symbol)
import safe Data.Cassie.Substitute (substitute, SubstitutionError)

type Steps = [String]

data SolverError
    = NeedsPolysolve
    | SymbolNotFound
    | IsolationErrorOccurred
    | FunctionNotUnderstood
    | ZeroOrSingleTermPolynomial
    | FailedToCallFunction SubstitutionError

instance Show SolverError where
    show NeedsPolysolve 
        = "the solution requires polysolving capability that is\
        \ not yet supported"
    
    show SymbolNotFound 
        = "the given symbol was not found in the given equation"
    
    show IsolationErrorOccurred 
        = "an unknown error occurred while trying to isolate the\
        \ given symbol in the equation"
    
    show FunctionNotUnderstood 
        = "encountered a function that could not be algebraically reversed"
    
    show ZeroOrSingleTermPolynomial 
        = "encountered a single-term polynomial expression. this\
        \ error should not be reached unless a manually-built\
        \ structure was isolated"

    show (FailedToCallFunction err)
        = "failed to call function on arguments: " ++ show err

type Isolate = RWST (Symbol, Context) Steps Equation (Except SolverError)

modifyRhs :: (AlgebraicStruct -> AlgebraicStruct) -> Isolate ()
modifyRhs f = do
    Equation (lhs, rhs) <- get
    put $ Equation (lhs, f rhs)

logStep :: Isolate ()
logStep = do
    step <- show <$> get
    tell [step]

setLhs :: AlgebraicStruct -> Isolate ()
setLhs lhs = do
    Equation (_, rhs) <- get
    put $ Equation (lhs, rhs)

isolate :: Equation -> Symbol -> Context -> Either SolverError (Equation, Steps)
isolate eqn sym ctx = runExcept $ execRWST isolateMain (sym, ctx) eqn

isolateMain :: Isolate ()
isolateMain = do
    Equation (lhs, rhs) <- get
    sym <- asks fst
    logStep
    if rhs ~? sym && not (lhs ~? sym) then do -- swap sides if needed
        setLhs rhs
        modifyRhs (const lhs)
        isolateMain
    else case lhs of
        Sum ts          -> isolateSum ts
        Difference ss   -> isolateDiff ss
        Product fs      -> isolateProd fs
        Quotient d s    -> isolateQuotient d s
        Exponent b e    -> isolateExp b e
        Logarithm b l   -> isolateLog b l
        Function n a    -> isolateFn n a
        Group g         -> setLhs g >> isolateMain
        Value _         -> lift $ throwError IsolationErrorOccurred
        Symbol s        -> if s == sym then 
                            return ()
                        else
                            lift $ throwError IsolationErrorOccurred

isolateSum :: [AlgebraicStruct] -> Isolate ()
isolateSum terms = do
    wrapperTerms <- isolatePolynomialTerm terms
    modifyRhs $ \rhs -> Difference [rhs, Group $ Sum wrapperTerms]
    isolateMain

isolateDiff :: [AlgebraicStruct] -> Isolate ()
isolateDiff (addend:subtrahends) = do
    sym <- asks fst
    if addend ~? sym then do -- need different behavior depending on where symbol is
        setLhs addend
        modifyRhs $ Sum . (:subtrahends)
    else do
        wrapperTerms <- isolatePolynomialTerm subtrahends
        modifyRhs $ Sum . (:addend:wrapperTerms)
    isolateMain
isolateDiff _ = lift $ throwError ZeroOrSingleTermPolynomial

isolateProd :: [AlgebraicStruct] -> Isolate ()
isolateProd factors = do
    wrapperTerms <- isolatePolynomialTerm factors
    modifyRhs $ \rhs -> Quotient (Group rhs) (Group $ Product wrapperTerms)
    isolateMain

isolateFn :: String -> [AlgebraicStruct] -> Isolate ()
isolateFn name callArgs = 
    let 
        applyFuncArgs an ca i = do 
            let result = foldM (flip $ uncurry substitute) i $ zip an ca
            case result of 
                Left err     -> lift . throwError $ FailedToCallFunction err
                Right subbed -> setLhs subbed
    in do
        ctx <- asks snd
        case Map.lookup name ctx of
            Nothing -> error ""
            Just (Const _) -> error ""
            Just (Func argNames impl)
                | length argNames == length callArgs 
                    -> applyFuncArgs argNames callArgs impl
                | otherwise -> error ""

isolateQuotient :: AlgebraicStruct -> AlgebraicStruct -> Isolate ()
isolateQuotient d s = 
    let 
        isolateDividend = do
            setLhs d
            modifyRhs $ Product . (:[s])

        isolateDivisor = do
            setLhs s 
            modifyRhs $ Quotient s
    in do 
        chooseBranch d s isolateDividend isolateDivisor
        isolateMain

isolateExp :: AlgebraicStruct -> AlgebraicStruct -> Isolate ()
isolateExp b e = 
    let 
        isolateBase = do
            setLhs b
            let rhsExp = Group $ Quotient (Value 1.0) e
            modifyRhs $ \rhs -> Exponent (Group rhs) (Group rhsExp)

        isolateExponent = do
            setLhs e
            modifyRhs $ Logarithm (Group b) . Group
    in do
        chooseBranch b e isolateBase isolateExponent 
        isolateMain
        
isolateLog :: AlgebraicStruct -> AlgebraicStruct -> Isolate ()
isolateLog b l = 
    let 
        isolateBase = do
            setLhs b
            let rhsExp = Group l
            modifyRhs $ Exponent rhsExp . Group . Quotient (Value 1.0)
        
        isolateLogarithm = do
            setLhs l
            modifyRhs $ Exponent b . Group
    in do
        chooseBranch b l isolateBase isolateLogarithm
        isolateMain

isolatePolynomialTerm :: [AlgebraicStruct] -> Isolate [AlgebraicStruct]
isolatePolynomialTerm terms = do
    sym <- asks fst
    let (wrapped, wrapper) = partition (~? sym) terms
    case wrapped of 
        [x] -> setLhs x >> return wrapper
        [] -> lift $ throwError SymbolNotFound
        _ -> lift $ throwError NeedsPolysolve

chooseBranch :: AlgebraicStruct -> AlgebraicStruct -> Isolate a -> Isolate a -> Isolate a
chooseBranch x y l r = do
    sym <- asks fst
    truthTable2 (~? sym) x y
        (lift $ throwError SymbolNotFound)
        (lift $ throwError NeedsPolysolve)
        l
        r
