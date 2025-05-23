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
import safe Data.List
import safe qualified Data.Map as Map
import safe qualified Data.List.NonEmpty as NonEmpty
import safe qualified Data.Set as Set
import safe Control.Monad.State (get, lift, modify, execStateT, StateT)
import safe Control.Monad.Except (runExcept, throwError, Except)
import safe Data.Cassie.Internal
import safe Data.Cassie.Evaluate (evaluate, isConst, EvalError)
import safe Data.Cassie.Isolate (isIsolated, isolate, IsolateError, Steps)
import safe Data.Cassie.Parser (parseEquation, CassieParserError)
import safe Data.Cassie.Parser.Lang (parseFunction, CassieLangError, Symbols)
import safe Data.Cassie.Structures (AlgStruct(..), Equation(..), RealCtx, CtxItem(..), RealEqn, Symbol)

-- | The Cassie 'compiler' monad for statefully building a 
--   solution to a system of equations.
type Cassie m u n = StateT (RealCtx, EquationPool m u n, Solution m u n) (Except CassieError)

type Solution m u n = Map.Map Symbol (SolutionValues m u n )

type SolutionValues m u n = (RealEqn, Steps, Either CassieError Double)

type EquationPool m u n = [(RealEqn, Symbols)]

data CassieError
    = ParseError CassieParserError
    | FunctionParseError CassieLangError
    | IsolationError IsolateError
    | EvaluationError EvalError
    | EvaluationArgError
    | ConstraintError String
    | FailedToFullySolve
    deriving Show

solvedFor :: String -> String -> RealCtx -> Either CassieError (RealEqn, Steps)
solvedFor eqn sym ctx = do
    (structure, syms) <- left ParseError $ parseEquation eqn
    when (not $ sym `Set.member` syms) 
        (Left . ConstraintError $ "target symbol did not exist in equation. found symbols: " ++ show syms)
    solution  <- left IsolationError $ isolate sym structure ctx
    return solution

solvedForValue :: String -> String -> RealCtx -> Either CassieError (Double, RealEqn, Steps)
solvedForValue eqn sym ctx = do
    (eqn', steps) <- solvedFor eqn sym ctx
    value' <- left EvaluationError $ evaluate (rhs eqn') ctx
    return (value', eqn', steps)

solveSystem :: String -> Either CassieError (RealCtx, Solution m u n)
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
            Left err -> lift (throwError $ IsolationError err)
            Right [] -> return False
            Right solved -> do 
                mapM_ addSolution solved
                return True

addSolution :: (RealEqn, Steps) -> Cassie m u n ()
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
    return ()

buildCtxAndEqnPool :: String -> Either CassieError (RealCtx, EquationPool m u n)
buildCtxAndEqnPool sys = do
    let (eqns, funcs) = partitionEqnsAndFuncs sys
    (consts, trueEqns) <- partitionConstsAndEquations <$> eqns
    ctx <- parseFunctions funcs =<< solveAndEvalConsts consts
    return (ctx, trueEqns)

partitionEqnsAndFuncs :: String -> (Either CassieError (EquationPool m u n), [String])
partitionEqnsAndFuncs = 
    let 
        f1a :: String -> ([String], [String])
        f1a = splitStrAt '\n'
            >>> map (NonEmpty.head . splitStrAt' '#')
            >>> filter isNonEmptyLine
            >>> partition ('=' `elem`)

        f1b :: [String] -> Either CassieError (EquationPool m u n)
        f1b = mapM $ left ParseError . parseEquation

    in f1a >>> first f1b

partitionConstsAndEquations :: EquationPool m u n -> ([(RealEqn, Symbol)], EquationPool m u n)
partitionConstsAndEquations = 
    let 
        f2a :: EquationPool m u n -> (EquationPool m u n, EquationPool m u n)
        f2a = partition $ \(eq, syms) -> (Set.size syms == 1) && (isIsolated eq $ Set.findMin syms)

        f2b :: EquationPool m u n -> [(RealEqn, Symbol)]
        f2b = map $ second Set.findMin

    in f2a >>> first f2b

solveAndEvalConsts :: [(RealEqn, Symbol)] -> Either CassieError (RealCtx)
solveAndEvalConsts xs = 
    let 
        f3 :: [(RealEqn, Symbol)] -> Either CassieError [RealEqn]
        f3 = mapM $ isolate' Map.empty
            >>> left IsolationError 
            >>> right fst

        f4a :: [RealEqn] -> Either CassieError (RealCtx)
        f4a = foldM f4b Map.empty

        f4b :: RealCtx -> RealEqn -> Either CassieError (RealCtx)
        f4b ctx eqn = do
            (x, result) <- evaluate' Map.empty eqn
            return $ Map.insert x (Const $ Nullary result) ctx

    in f3 xs >>= f4a

parseFunctions :: [String] -> RealCtx -> Either CassieError (RealCtx)
parseFunctions funcs ctx = (left FunctionParseError) $ foldM parseFunction ctx funcs 

isolate' :: RealCtx -> (RealEqn, Symbol) -> Either IsolateError (RealEqn, Steps)
isolate' ctx (eqn, sym) = isolate sym eqn ctx

evaluate' :: RealCtx -> RealEqn -> Either CassieError (Symbol, Double)
evaluate' ctx (Equation (Symbol x) rhs') = do
    result <- left EvaluationError $ evaluate rhs' ctx
    return (x, result)
evaluate' _ _ = Left EvaluationArgError

getCtx :: Cassie m u n (RealCtx)
getCtx = let f (x, _, _) = x in f <$> get

getEqns :: Cassie  m u n (EquationPool m u n)
getEqns = let f (_, x, _) = x in f <$> get

getKnownConsts :: Cassie m u n Symbols
getKnownConsts = do
    ctx <- getCtx
    let f = isConst . (ctx Map.!)
    return $ Set.fromList . filter f . Map.keys $ ctx

modifyCtx :: (RealCtx -> RealCtx) -> Cassie m u n ()
modifyCtx f = modify $ first' f

modifyEqns :: (EquationPool m u n -> EquationPool m u n) -> Cassie m u n ()
modifyEqns f = modify $ second' f

addSoln :: Symbol -> SolutionValues m u n -> Cassie m u n ()
addSoln sym sv = modify (third $ Map.insert sym sv)

showStepsFor :: Symbol -> Solution m u n -> IO () 
showStepsFor name soln = 
    let 
        getSteps = do
            (_, x, _) <- Map.lookup name soln
            return x
    in 
        putStrLn $ show (intercalate "\n" <$> getSteps)

getSymbol :: AlgStruct m u n -> Symbol
getSymbol (Symbol x) = x
getSymbol _ = error "given structure was not a symbol"
