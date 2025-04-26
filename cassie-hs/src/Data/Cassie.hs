{-# LANGUAGE Safe #-}
module Data.Cassie 
    ( solvedFor
    , solvedForValue
    , solveSystem
    -- , solveSystemNumerically
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Data.List
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set
import safe Control.Monad.State (get, lift, modify, execStateT, StateT)
import safe Control.Monad.Except (runExcept, throwError, Except)
import safe Data.Cassie.Internal
import safe Data.Cassie.Evaluate (evaluate, isConst, Context, CtxItem(..), EvalError)
import safe Data.Cassie.Isolate (isolate, Steps, IsolateError)
import safe Data.Cassie.Parser (parseEquation, CassieParserError)
import safe Data.Cassie.Parser.Lang (parseFunction, CassieLangError)
import safe Data.Cassie.Structures (getSymbol, leftHand, rightHand, AlgebraicStruct(..), Equation(..), Symbol)

-- | The Cassie 'compiler' monad for statefully building a 
--   solution to a system of equations.
type Cassie = StateT (Context, EquationPool, Solution) (Except CassieError)

type Solution = Map.Map Symbol SolutionValues

type SolutionValues = (Equation, Steps, Either CassieError Double)

type EquationPool = [(Equation, Symbols)]

type Symbols = Set.Set Symbol

data CassieError
    = ParseError CassieParserError
    | FunctionParseError CassieLangError
    | IsolationError IsolateError
    | EvaluationError EvalError
    | EvaluationArgError
    | ConstraintError String
    | FailedToFullySolve
    deriving Show

solvedFor :: String -> String -> Context -> Either CassieError (Equation, Steps)
solvedFor eqn sym ctx = do
    (structure, syms) <- left ParseError $ parseEquation eqn
    when (not $ sym `Set.member` syms) 
        (Left . ConstraintError $ "target symbol did not exist in equation. found symbols: " ++ show syms)
    solution  <- left IsolationError $ isolate structure sym ctx
    return solution

solvedForValue :: String -> String -> Context -> Either CassieError (Double, Equation, Steps)
solvedForValue eqn sym ctx = do
    (eqn', steps) <- solvedFor eqn sym ctx 
    let Equation (_, value) = eqn'
    value' <- left EvaluationError $ evaluate value ctx
    return (value', eqn', steps)

solveSystem :: String -> Either CassieError Solution
solveSystem sys = do
        (ctx, eqns) <- buildCtxAndEqnPool sys
        (_, _, solnInfo) <- runExcept $ execStateT solveSystemMain (ctx, eqns, Map.empty)
        return solnInfo

solveSystemMain :: Cassie ()
solveSystemMain = do
    updateUnknowns
    madeProgress <- solveSingleUnknowns
    if madeProgress then
        solveSystemMain
    else do
        solvedSubsystem <- solveSystems
        eqnPool <- getEqns
        if solvedSubsystem then
            solveSystemMain
        else if length eqnPool /= 0 then
            lift $ throwError FailedToFullySolve
        else
            return ()

-- | BIG TODO HERE - requires linear system substitution solver
solveSystems :: Cassie Bool
solveSystems = error "not implemented yet"

solveSingleUnknowns :: Cassie Bool
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

addSolution :: (Equation, Steps) -> Cassie ()
addSolution (eqn, steps) = do
    let name = getSymbol $ leftHand eqn
    let value = rightHand eqn 
    numSoln <- flip evaluate' eqn <$> getCtx
    modifyCtx $ Map.insert name (Const value)
    addSoln name (eqn, steps, snd <$> numSoln)
    
updateUnknowns :: Cassie ()
updateUnknowns = do
    knowns <- getKnownConsts
    let f = second (`Set.difference` knowns)
    modifyEqns $ map f
    return ()

buildCtxAndEqnPool :: String -> Either CassieError (Context, EquationPool)
buildCtxAndEqnPool sys = do
    let (eqns, funcs) = partitionEqnsAndFuncs sys
    (consts, trueEqns) <- partitionConstsAndEquations <$> eqns
    ctx <- parseFunctions funcs =<< solveAndEvalConsts consts
    return (ctx, trueEqns)

partitionEqnsAndFuncs :: String -> (Either CassieError EquationPool, [String])
partitionEqnsAndFuncs = 
    let 
        f1a ::String -> ([String], [String])
        f1a = partition ('=' `elem`) . splitStrAt '\n'

        f1b :: [String] -> Either CassieError EquationPool
        f1b = mapM $ left ParseError . parseEquation

    in f1a >>> first f1b

partitionConstsAndEquations :: EquationPool -> ([(Equation, Symbol)], EquationPool)
partitionConstsAndEquations = 
    let 
        f2a :: EquationPool -> (EquationPool, EquationPool)
        f2a = partition $ (== 1) . Set.size . snd

        f2b :: EquationPool -> [(Equation, Symbol)]
        f2b = map $ second Set.findMin

    in f2a >>> first f2b

solveAndEvalConsts :: [(Equation, Symbol)] -> Either CassieError Context
solveAndEvalConsts xs = 
    let 
        f3 :: [(Equation, Symbol)] -> Either CassieError [Equation]
        f3 = mapM $ isolate' Map.empty
            >>> left IsolationError 
            >>> right fst

        f4a :: [Equation] -> Either CassieError Context
        f4a = foldM f4b Map.empty

        f4b :: Context -> Equation -> Either CassieError Context
        f4b ctx eqn = do
            (x, result) <- evaluate' Map.empty eqn
            return $ Map.insert x (Const $ Value result) ctx

    in f3 xs >>= f4a

parseFunctions :: [String] -> Context -> Either CassieError Context
parseFunctions funcs ctx = (left FunctionParseError) $ foldM parseFunction ctx funcs 

isolate' :: Context -> (Equation, Symbol) -> Either IsolateError (Equation, Steps)
isolate' ctx = flip (uncurry isolate) ctx

evaluate' :: Context -> Equation -> Either CassieError (Symbol, Double)
evaluate' ctx (Equation (Symbol x, rhs')) = do
    result <- left EvaluationError $ evaluate rhs' ctx
    return (x, result)
evaluate' _ _ = Left EvaluationArgError

getCtx :: Cassie Context
getCtx = let f (x, _, _) = x in f <$> get

getEqns :: Cassie EquationPool
getEqns = let f (_, x, _) = x in f <$> get

getKnownConsts :: Cassie Symbols
getKnownConsts = do
    ctx <- getCtx
    let f = isConst . (ctx Map.!)
    return $ Set.fromList . filter f . Map.keys $ ctx

modifyCtx :: (Context -> Context) -> Cassie ()
modifyCtx f = modify $ first' f

modifyEqns :: (EquationPool -> EquationPool) -> Cassie ()
modifyEqns f = modify $ second' f

addSoln :: Symbol -> SolutionValues -> Cassie ()
addSoln sym sv = modify (third $ Map.insert sym sv)
