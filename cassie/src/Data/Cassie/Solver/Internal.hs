{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cassie.Solver.Internal
    ( solveConstrainedMain
    , Cassie
    , CassieT
    , CassieError(..)
    , EquationPool
    , Solution
    , SolutionItem(..)
    , Symbols
    ) where

import safe Control.Arrow
import safe Control.Monad.Identity (Identity)
import safe Control.Monad.Except (ExceptT)
import safe Control.Monad.RWS (ask, get, modify, RWST)
import safe Data.Cassie.CLI.Module.Internal (CassieModuleError)
import safe Data.Cassie.CLI.Parser.Internal (CassieParserError)
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Rules.Isolate
import safe Data.Cassie.Structures
import safe Data.Cassie.Utils
import safe Data.List as List 
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set

-- | The Cassie 'compiler' monad for statefully building a 
--   solution to a system of equations.
type Cassie mg u n = CassieT mg u n Identity

-- | 
type CassieT mg u n m = RWST (Context mg u n) () (Solution mg u n, EquationPool mg u n) (ExceptT (CassieError mg u n) m)

-- | A type alias for a map of equations (possibly with repeated keys)
type EquationPool mg u n = [(Equation mg u n, Symbols)]

-- | Similar to @Context mg u n@, but contains imformation about the 
--   symbolic solutions gleaned from a system of equations.
type Solution mg u n = Map.Map Symbol (SolutionItem mg u n)

-- | Type alias for a set of symbols
type Symbols = Set.Set Symbol

-- | Similar to @CtxItem@, but contains metadata not needed by the 
--   @evalulate@ function. 
data SolutionItem mg u n = SolutionItem
    { constrained :: Equation mg u n
    , steps       :: Steps
    , possVal     :: Either EvalError n
    }
    deriving (Show, Eq)

data CassieError mg u n
    = ConstraintError String
    | EvaluationArgError
    | EvaluationError EvalError
    | FailedToConstrain (EquationPool mg u n)
    | FailedToFullySolve
    | ImportsNotAllowed
    | ImportNotFound (String, [Symbols])
    | IsolationError IsolateError
    {-  Errors on or below this line are thrown by the module system only.
        This is confusing, but allows @solveConstrainedMain@ to be called 
        directly within the module-importing code since both actions use
        the same @ExceptT e@ monad. -}
    | ParserError CassieParserError
    | ImportError CassieModuleError
    deriving (Show, Eq)

solveConstrainedMain :: (Monad m, AlgebraicStructure mg u n) => CassieT mg u n m ()
solveConstrainedMain = do
    updateUnknowns
    madeProgress <- solveSingleUnknowns
    if madeProgress then
        solveConstrainedMain
    else
        return ()
        -- TODO: put subsystem solver call here 

-- | Solves equations that only have one DOF when
solveSingleUnknowns :: (Monad m, AlgebraicStructure mg u n) => CassieT mg u n m Bool
solveSingleUnknowns = 
    let
        isolate' ctx = uncurry (isolate ctx) . second Set.findMin
        createSolnItem ctx soln = uncurry SolutionItem soln (evaluate ctx . rhs . fst $ soln)
        getSolnData ctx = map $ getSymbol . lhs . fst &&& createSolnItem ctx
    in do
        ctx <- getCtx
        (cnstrnd, remaining) <- getConstrained
        if cnstrnd == [] then
            return False
        else do
            isoSolutions <- throwErr . IsolationError ||| pure 
                $ mapM (isolate' ctx) cnstrnd
            modifySoln . Map.union . Map.fromList . getSolnData ctx $ isoSolutions
            modifyEqns $ const remaining
            return True

getConstrained :: Monad m => CassieT mg u n m (EquationPool mg u n, EquationPool mg u n)
getConstrained = (partition $ (== 1) . Set.size . snd) <$> getEqns

updateUnknowns :: Monad m => CassieT mg u n m ()
updateUnknowns = do
    knowns <- getKnowns
    modifyEqns . map $ second (`Set.difference` knowns)

getCtx :: Monad m => CassieT mg u n m (Context mg u n)
getCtx = ask 

getSolved :: Monad m => CassieT mg u n m (Solution mg u n)
getSolved = fst <$> get

getEqns :: Monad m => CassieT mg u n m (EquationPool mg u n)
getEqns = snd <$> get

-- | Returns the known VALUES in the solver's numeric context map.
getKnowns :: Monad m => CassieT mg u n m Symbols
getKnowns = do
    ctx <- getSolved
    let f = (`Map.member` ctx)
    return $ Set.fromList . filter f . Map.keys $ ctx

modifySoln :: Monad m =>  (Solution mg u n -> Solution mg u n) -> CassieT mg u n m ()
modifySoln f = do
    -- initial <- getSolved
    modify $ first f
    -- (initial ==) <$> getSolved

modifyEqns :: Monad m =>  (EquationPool mg u n -> EquationPool mg u n) -> CassieT mg u n m ()
modifyEqns f = modify $ second f

getSymbol :: (ShowMagma mg, ShowUnary u, Show n, Num n) => AlgStruct mg u n -> Symbol
getSymbol (Symbol x) = x
getSymbol x = error $ "given structure '" ++ showAlgStruct x ++ "' was not a symbol"
