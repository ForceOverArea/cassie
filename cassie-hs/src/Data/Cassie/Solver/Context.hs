{-# LANGUAGE Safe #-}
module Data.Cassie.Solver.Context
    ( buildGlobalCtx
    , evaluate'
    , isolate'
    ) where

import safe Control.Arrow
-- import safe Control.Monad
import safe Data.Cassie.Parser.Lang 
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Rules.Isolate
import safe Data.Cassie.Solver.Internal
import safe Data.Cassie.Structures (Equation(..))
import safe Data.Cassie.Structures.Internal (AlgStruct(Symbol), Symbol)

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
