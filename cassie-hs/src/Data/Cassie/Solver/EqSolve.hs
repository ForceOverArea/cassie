{-# LANGUAGE Safe #-}
module Data.Cassie.Solver.EqSolve
    ( solveSystemMain
    , showStepsFor
    ) where

import safe Control.Arrow
import safe Control.Monad.State (get, modify)
import safe Data.Cassie.Parser.Internal
import safe Data.Cassie.Parser.Lang
import safe Data.Cassie.Rules.Isolate
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Solver.Internal
import safe Data.Cassie.Solver.Context
import safe Data.Cassie.Structures
import safe Data.Cassie.Utils
import safe Data.List as List 
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set

solveSystemMain :: Cassie m u n ()
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

solveSingleUnknowns :: Cassie m u n Bool
solveSingleUnknowns = 
    let
        isConstrained = (1 ==) . Set.size . snd
        solve1Unknown ctx = isolate' ctx . second Set.findMin
    in do
        ctx <- getCtx
        constrained <- filter isConstrained <$> getEqns
        case mapM (solve1Unknown ctx) constrained of
            Left err -> throwErr $ IsolationError err
            Right [] -> return False
            Right solved -> do
                mapM_ addSolution solved
                return True

addSolution :: (ParsedEqn, Steps) -> Cassie m u n ()
addSolution (eqn, steps) = do
    let (name, value) = (getSymbol . lhs &&& rhs) eqn
    numSoln <- flip evaluate' eqn <$> getCtx
    deps <- Set.intersection (getSyms value) <$> getKnowns
    modifyCtx $ Map.insert name (Known value deps)
    addSoln name (eqn, steps, snd <$> numSoln)
    
updateUnknowns :: Cassie m u n ()
updateUnknowns = do
    knowns <- getKnownConsts
    let f = second (`Set.difference` knowns)
    modifyEqns $ map f

getCtx :: Cassie m u n ParsedCtx
getCtx = let f (x, _, _) = x in f <$> get

getKnowns :: Cassie m u n Symbols
getKnowns = Set.fromList . Map.keys <$> getCtx

getEqns :: Cassie  m u n EquationPool
getEqns = let f (_, x, _) = x in f <$> get

getKnownConsts :: Cassie m u n Symbols
getKnownConsts = do
    ctx <- getCtx
    let f = isConst . (ctx Map.!)
    return $ Set.fromList . filter f . Map.keys $ ctx

modifyCtx :: (ParsedCtx -> ParsedCtx) -> Cassie m u n ()
modifyCtx f = modify $ first' f

modifyEqns :: (EquationPool -> EquationPool) -> Cassie m u n ()
modifyEqns f = modify $ second' f

addSoln :: Symbol -> SolutionValues -> Cassie m u n ()
addSoln sym sv = modify (third $ Map.insert sym sv)

showStepsFor :: Symbol -> Solution -> IO () 
showStepsFor name soln = 
    let 
        getSteps = do
            (_, x, _) <- Map.lookup name soln
            return x
    in 
        putStrLn $ show (intercalate "\n" <$> getSteps)

getSymbol :: (ShowMagma m, ShowUnary u, Show n, Num n) => AlgStruct m u n -> Symbol
getSymbol (Symbol x) = x
getSymbol x = error $ "given structure '" ++ showAlgStruct x ++ "' was not a symbol"
