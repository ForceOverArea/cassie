{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cassie.Solver.Internal
    ( buildGlobalCtx
    , showStepsFor
    , solveSystemMain
    , Cassie
    , CassieT
    , CassieError(..)
    , EquationPool
    , Solution
    , SolutionItem(..)
    ) where

import safe Control.Arrow
import safe Control.Monad.Identity (Identity)
import safe Control.Monad.Except (ExceptT)
import safe Control.Monad.State (StateT)
import safe Control.Monad.Trans (lift)
import safe Control.Monad.Except (throwError)
import safe Control.Monad.State (get, modify)
import safe Data.Cassie.Parser.Internal
import safe Data.Cassie.Parser.Lang
import safe Data.Cassie.Rules.Isolate
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Structures
import safe Data.Cassie.Utils
import safe Data.List as List 
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set

-- | The Cassie 'compiler' monad for statefully building a 
--   solution to a system of equations.
type Cassie m u n = CassieT m u n Identity

type CassieT mg u n m = StateT (ParsedCtx, EquationPool, Solution) (ExceptT CassieError m)

type Solution = Map.Map Symbol SolutionItem

-- | Similar to @CtxItem@, but contains metadata not needed by the 
--   @evalulate@ function. 
data SolutionItem = SolutionItem
    { eqn     :: ParsedEqn
    , steps   :: Steps
    , possVal :: Either CassieError Double
    }
    deriving (Show, Eq)

data CassieError
    = ConstraintError String
    | EvaluationArgError
    | EvaluationError EvalError
    | FailedToConstrain EquationPool
    | FailedToFullySolve
    | FileDoesNotExist FilePath
    | FoundRecursiveImport
    | IsolationError IsolateError
    | ImportsNotAllowed
    | ImportNotFound (String, [Symbols])
    | ParseError CassieParserError
    deriving (Show, Eq)

solveSystemMain :: Monad m => CassieT mg u n m ()
solveSystemMain = do
    updateUnknowns
    madeProgress <- solveSingleUnknowns
    if madeProgress then
        solveSystemMain
    else do
        return ()
        -- solvedSubsystem <- solveSystems
        -- eqnPool <- getEqns
        -- if solvedSubsystem then
        --     solveSystemMain
        -- else if length eqnPool /= 0 then
        --     lift $ throwError FailedToFullySolve
        -- else
        --     return ()

-- | BIG TODO HERE - requires linear system substitution solver
-- solveSystems :: Cassie Bool
-- solveSystems = error "not implemented yet"

solveSingleUnknowns :: Monad m => CassieT mg u n m Bool
solveSingleUnknowns = 
    let
        isConstrained = (1 ==) . Set.size . snd
        solve1Unknown ctx = isolate' ctx . second Set.findMin
    in do
        ctx <- getCtx
        constrained <- filter isConstrained <$> getEqns
        case mapM (solve1Unknown ctx) constrained of
            Left err -> lift . throwError $ IsolationError err
            Right [] -> return False
            Right solved -> do
                mapM_ addSolution solved
                return True

-- | Builds the global context of a system prior to solving it. This 
--   consists of 3 things:
--
--   1. A list of imported modules that the file (may) depend on
--   2. A @Context@ map structure of symbols to functions or constant values
--   3. A pool of equations parsed out in the system. 
buildGlobalCtx :: FilePath -> String -> Either CassieError ([Import], ParsedCtx, EquationPool)
buildGlobalCtx = curry (left ParseError . uncurry parseCassiePhrases)

-- | A refactored version of @isolate@ that plays nicer with the types used in the @Data.Cassie.Solver@ modules.
isolate' :: ParsedCtx -> (ParsedEqn, Symbol) -> Either IsolateError (ParsedEqn, Steps)
isolate' ctx (unsolved, sym) = isolate sym unsolved ctx

-- | A refactored version of @evaluate@ that plays nicer with the types used in the @Data.Cassie.Solver@ modules.
evaluate' :: ParsedCtx -> ParsedEqn -> Either CassieError (Symbol, Double)
evaluate' ctx (Equation (Symbol x) rhs') = ((const x &&& id) <$>) . left EvaluationError $ evaluate rhs' ctx
evaluate' _ _ = Left EvaluationArgError

addSolution :: Monad m => (ParsedEqn, Steps) -> CassieT mg u n m ()
addSolution (eqn', steps') = do
    let (name, value) = (getSymbol . lhs &&& rhs) eqn'
    numSoln <- flip evaluate' eqn' <$> getCtx
    deps <- Set.intersection (getSyms value) <$> getKnowns
    modifyCtx $ Map.insert name (Known value deps)
    addSoln name . SolutionItem eqn' steps' $ snd <$> numSoln
    
updateUnknowns :: Monad m => CassieT mg u n m ()
updateUnknowns = do
    knowns <- getKnownConsts
    let f = second (`Set.difference` knowns)
    modifyEqns $ map f

getCtx :: Monad m => CassieT mg u n m ParsedCtx
getCtx = let f (x, _, _) = x in f <$> get

getKnowns :: Monad m => CassieT mg u n m Symbols
getKnowns = Set.fromList . Map.keys <$> getCtx

getEqns :: Monad m => CassieT mg u n m EquationPool
getEqns = let f (_, x, _) = x in f <$> get

getKnownConsts :: Monad m => CassieT mg u n m Symbols
getKnownConsts = do
    ctx <- getCtx
    let f = isConst . (ctx Map.!)
    return $ Set.fromList . filter f . Map.keys $ ctx

modifyCtx :: Monad m => (ParsedCtx -> ParsedCtx) -> CassieT mg u n m ()
modifyCtx f = modify $ first' f

modifyEqns :: Monad m =>  (EquationPool -> EquationPool) -> CassieT mg u n m ()
modifyEqns f = modify $ second' f

addSoln :: Monad m => Symbol -> SolutionItem -> CassieT mg u n m ()
addSoln sym si = modify (third $ Map.insert sym si)

showStepsFor :: Symbol -> Solution -> IO () 
showStepsFor name soln = 
    let 
        getSteps = steps <$> Map.lookup name soln
    in 
        putStrLn $ show (intercalate "\n" <$> getSteps)

getSymbol :: (ShowMagma m, ShowUnary u, Show n, Num n) => AlgStruct m u n -> Symbol
getSymbol (Symbol x) = x
getSymbol x = error $ "given structure '" ++ showAlgStruct x ++ "' was not a symbol"
