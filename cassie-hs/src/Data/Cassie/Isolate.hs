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
    , IsolateError
    ) where

import safe Data.List
import safe qualified Data.Map as Map
import safe Control.Monad.RWS (asks, get, put, tell, execRWST, RWST(..))
import safe Control.Monad.Except (runExcept, Except)
import safe Data.Cassie.Evaluate (Context, CtxItem(..))
import safe Data.Cassie.Internal
import safe Data.Cassie.Structures ((~?), AlgebraicStruct(..), Equation (Equation), Symbol)
import safe Data.Cassie.Substitute (substituteFuncArgs, SubstitutionError)

-- | A type alias for the log of steps taken while solving a given equation. 
type Steps = [String]

-- | An error type that may be thrown when attempting to solve an equation
--   by symbolic isolation.
data IsolateError
    = NeedsPolysolve
    | SymbolNotFound String
    | IsolationErrorOccurred
    | FunctionArgumentsIncorrect Int Int
    | ZeroOrSingleTermPolynomial
    | FailedToCallFunction SubstitutionError
    | NotAFunction String

instance Show IsolateError where
    show NeedsPolysolve 
        = "the solution requires polysolving capability that is\
        \ not yet supported"
    
    show (SymbolNotFound name)
        = "the given symbol '" ++ name ++ "' was not found in the given equation"
    
    show IsolationErrorOccurred 
        = "an unknown error occurred while trying to isolate the\
        \ given symbol in the equation"
    
    show (FunctionArgumentsIncorrect expected actual)
        = "The function was called with the incorrect number of\
        \ arguments (expected " 
        ++ show expected
        ++ ", found "
        ++ show actual
        ++")"

    show ZeroOrSingleTermPolynomial 
        = "encountered a single-term polynomial expression. this\
        \ error should not be reached unless a manually-built\
        \ structure was isolated"

    show (FailedToCallFunction err)
        = "failed to call function on arguments: " ++ show err

    show (NotAFunction name)
        = "the symbol '" ++ name ++ "' is not a function"

-- | The @Isolate@ monad type for statefully isolating a variable in a 
--   larger expression.
type Isolate = RWST (Symbol, Context) Steps Equation (Except IsolateError)

-- | Applies a function to the right-hand side of the equation being solved,
--   changing its state. This function is roughly a more concrete instance of
--   @Control.Monad.State.modify@.
modifyRhs :: (AlgebraicStruct -> AlgebraicStruct) -> Isolate ()
modifyRhs f = do
    Equation (lhs, rhs) <- get
    put $ Equation (lhs, f rhs)

-- | Puts a snapshot of the current state of the equation being solved into the 
--   log of steps taken in the solution. 
logStep :: Isolate ()
logStep = do
    step <- show <$> get
    tell [step]

-- | Sets the state of the left-hand side of the equation being solved. This 
--   function is roughly a more concrete instance of @Control.Monad.State.put@.
setLhs :: AlgebraicStruct -> Isolate ()
setLhs lhs = do
    Equation (_, rhs) <- get
    put $ Equation (lhs, rhs)

-- | Attempts to isolate a given symbol @sym@ in a given @Equation@, either returning
--   a solved equation and the steps taken to achieve the solution, or a @IsolateError@.
isolate :: Equation -> Symbol -> Context -> Either IsolateError (Equation, Steps)
isolate eqn sym ctx = runExcept $ execRWST isolateMain (sym, ctx) eqn

-- | The main control flow for isolating a symbol in an algebraic structure.
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
        Value _         -> throwM IsolationErrorOccurred
        Symbol s
            | s == sym  -> return ()
            | otherwise -> throwM IsolationErrorOccurred

-- | Control flow for isolating the target symbol in some number of terms.
isolateSum :: [AlgebraicStruct] -> Isolate ()
isolateSum terms = do
    wrapperTerms <- isolatePolynomialTerm terms
    modifyRhs $ \rhs -> Difference [rhs, Group $ Sum wrapperTerms]
    isolateMain

-- | Control flow for isolating the target symbol in some number of subtrahends.
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
isolateDiff _ = throwM ZeroOrSingleTermPolynomial

-- | Control flow for isolating the target symbol in some number of factors.
isolateProd :: [AlgebraicStruct] -> Isolate ()
isolateProd factors = do
    wrapperTerms <- isolatePolynomialTerm factors
    modifyRhs $ \rhs -> Quotient (Group rhs) (Group $ Product wrapperTerms)
    isolateMain

-- | Control flow for isolating some symbol that has been passed to a function.
isolateFn :: String -> [AlgebraicStruct] -> Isolate ()
isolateFn name callSiteArgs = do
    ctx <- asks snd
    case Map.lookup name ctx of
        Nothing        -> throwM $ SymbolNotFound name
        Just (Const _) -> throwM $ NotAFunction name
        Just (Func argNames impl)
            | length argNames /= length callSiteArgs 
                -> throwM $ FunctionArgumentsIncorrect (length argNames) (length callSiteArgs)
            | otherwise 
                -> case substituteFuncArgs impl argNames callSiteArgs of
                    Left err -> throwM $ FailedToCallFunction err
                    Right x  -> setLhs x
    isolateMain

-- | Control flow for isolating the target symbol within a quotient.
isolateQuotient :: AlgebraicStruct -> AlgebraicStruct -> Isolate ()
isolateQuotient d s = 
    let 
        isolateDividend = do
            setLhs d
            modifyRhs $ Product . (:[s])

        isolateDivisor = do
            setLhs s 
            modifyRhs $ Quotient d
    in do 
        chooseBranch d s isolateDividend isolateDivisor
        isolateMain

-- | Control flow for isolating the target symbol within an exponent.
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

-- | Control flow for isolating the target symbol within a logarithm.
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

-- | Helper function for isolating the target symbol in a many-termed expression.
isolatePolynomialTerm :: [AlgebraicStruct] -> Isolate [AlgebraicStruct]
isolatePolynomialTerm terms = do
    sym <- asks fst
    let (wrapped, wrapper) = partition (~? sym) terms
    case wrapped of 
        [x] -> setLhs x >> return wrapper
        [] -> throwM $ SymbolNotFound sym
        _ -> throwM NeedsPolysolve

-- | Helper function for isolating the target symbol in the arguments of a binary algebraic structure.
chooseBranch :: AlgebraicStruct -> AlgebraicStruct -> Isolate a -> Isolate a -> Isolate a
chooseBranch x y l r = do
    sym <- asks fst
    truthTable2 (~? sym) x y
        (throwM $ SymbolNotFound sym)
        (throwM $ NeedsPolysolve)
        l
        r

