{-# LANGUAGE Safe #-}
module Data.Cassie 
    ( solvedFor
    , solvedForValue
    , solveSystem
    , showStepsFor
    , CassieError
    , Solution
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Except (runExcept, Except)
import safe Control.Monad.State (get, modify, execStateT, StateT)
import safe Data.Cassie.Parser.Internal (CassieParserError)
import safe Data.Cassie.Parser.Lang
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Rules.Isolate
import safe Data.Cassie.Structures
import safe Data.Cassie.Utils
import safe qualified Data.Map as Map
import safe Data.List
import safe qualified Data.Set as Set

-- | The Cassie 'compiler' monad for statefully building a 
--   solution to a system of equations.
type Cassie m u n = StateT (Context RealMagma RealUnary Double, EquationPool, Solution) (Except CassieError)

type Solution = Map.Map Symbol SolutionValues

type SolutionValues = (ParsedEqn, Steps, Either CassieError Double)

type EquationPool = [(ParsedEqn, Symbols)]

data CassieError
    = ParseError CassieParserError
    | IsolationError IsolateError
    | EvaluationError EvalError
    | EvaluationArgError
    | ConstraintError String
    | FailedToFullySolve
    deriving Show

solvedFor :: String -> String -> ParsedCtx -> Either CassieError (ParsedEqn, Steps)
solvedFor eqn sym ctx = do
    (structure, syms) <- left ParseError $ parseEquation' eqn
    when (not $ sym `Set.member` syms) 
        (Left . ConstraintError $ "target symbol did not exist in equation. found symbols: " ++ show syms)
    solution <- left IsolationError $ isolate sym structure ctx
    return solution

solvedForValue :: String -> String -> ParsedCtx -> Either CassieError (Double, ParsedEqn, Steps)
solvedForValue eqn sym ctx = do
    (eqn', steps) <- solvedFor eqn sym ctx
    value' <- left EvaluationError $ evaluate (rhs eqn') ctx
    return (value', eqn', steps)

solveSystem :: String -> Either CassieError (ParsedCtx, Solution)
solveSystem sys = do
    (ctx, eqns) <- buildCtxAndEqnPool sys
    (_, _, solnInfo) <- runExcept $ execStateT solveSystemMain (ctx, eqns, Map.empty)
    return (ctx, solnInfo)

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
    let name = getSymbol $ lhs eqn
    let value = rhs eqn 
    numSoln <- flip evaluate' eqn <$> getCtx
    modifyCtx $ Map.insert name (Const value)
    addSoln name (eqn, steps, snd <$> numSoln)
    
updateUnknowns :: Cassie m u n ()
updateUnknowns = do
    knowns <- getKnownConsts
    let f = second (`Set.difference` knowns)
    modifyEqns $ map f

buildCtxAndEqnPool :: String -> Either CassieError (ParsedCtx, EquationPool)
buildCtxAndEqnPool sys = do
    (eqns, funcs) <- partitionEqnsAndFuncs sys
    let (consts, trueEqns) = partitionConstsAndEquations eqns
    ctx <- Map.union funcs <$> solveAndEvalConsts consts
    return (ctx, trueEqns)

partitionEqnsAndFuncs :: String -> Either CassieError (EquationPool, ParsedCtx)
partitionEqnsAndFuncs source = left ParseError $ parseCassiePhrases "system" source -- TODO: find way to inject filenames here

partitionConstsAndEquations :: EquationPool -> ([(ParsedEqn, Symbol)], EquationPool)
partitionConstsAndEquations = 
    let 
        f2a :: EquationPool -> (EquationPool, EquationPool)
        f2a = partition $ \(eq, syms) -> (Set.size syms == 1) && (isIsolated eq $ Set.findMin syms)

        f2b :: EquationPool -> [(ParsedEqn, Symbol)]
        f2b = map $ second Set.findMin

    in f2a >>> first f2b

solveAndEvalConsts :: [(ParsedEqn, Symbol)] -> Either CassieError (ParsedCtx)
solveAndEvalConsts xs = 
    let 
        f3 :: [(ParsedEqn, Symbol)] -> Either CassieError [ParsedEqn]
        f3 = mapM $ isolate' Map.empty
            >>> left IsolationError 
            >>> right fst

        f4a :: [ParsedEqn] -> Either CassieError (ParsedCtx)
        f4a = foldM f4b Map.empty

        f4b :: ParsedCtx -> ParsedEqn -> Either CassieError (ParsedCtx)
        f4b ctx eqn = do
            (x, result) <- evaluate' Map.empty eqn
            return $ Map.insert x (Const $ Nullary result) ctx

    in f3 xs >>= f4a

isolate' :: ParsedCtx -> (ParsedEqn, Symbol) -> Either IsolateError (ParsedEqn, Steps)
isolate' ctx (eqn, sym) = isolate sym eqn ctx

evaluate' :: ParsedCtx -> ParsedEqn -> Either CassieError (Symbol, Double)
evaluate' ctx (Equation (Symbol x) rhs') = do
    result <- left EvaluationError $ evaluate rhs' ctx
    return (x, result)
evaluate' _ _ = Left EvaluationArgError

getCtx :: Cassie m u n (ParsedCtx)
getCtx = let f (x, _, _) = x in f <$> get

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
