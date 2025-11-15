{-# LANGUAGE Safe #-}
module Data.Cassie.Solver.CtxEval
    ( strictEvalCtx
    ) where

import safe Control.Monad.Except
import safe Data.Cassie.Rules
import safe Data.Cassie.Solver.Internal
import safe Data.Cassie.Structures
import safe qualified Data.Map as Map

strictEvalCtx :: (Monad m, AlgebraicStructure mg u n) => Context mg u n -> ExceptT (CassieError mg u n) m (Context mg u n)
strictEvalCtx = strictEvalCtxHelper 0

-- | Ensures that the context being referenced by a solution is self-sufficient. 
--   If this step is not completed prior to attempting a solution, then the 
--   algorithm may return an error for context items who have an undetected
--   dependency on an undefined symbol. 
strictEvalCtxHelper :: (Monad m, AlgebraicStructure mg u n) => Int -> Context mg u n -> ExceptT (CassieError mg u n) m (Context mg u n)
strictEvalCtxHelper count ctx = 
    let
        concreteCtx = Map.foldlWithKey addConcreteValueToCtx mempty ctx

        addConcreteValueToCtx ctxAcc sym expr = 
            if isConst expr && (not $ sym `Map.member` ctxAcc) then -- 
                case evaluate ctxAcc $ numeric expr of
                    Left _err -> ctxAcc -- ignore things we can't know yet
                    Right ans -> Map.insert sym (Known (Nullary ans) mempty) ctxAcc
            else -- no need to evaluate functions or values we already know
                ctxAcc

    in if count > Map.size ctx then
        throwError . ContextMissingFor $ Map.keys ctx
    else if Map.keys concreteCtx == Map.keys ctx then
        pure concreteCtx
    else
        strictEvalCtxHelper (count + 1) $ concreteCtx `Map.union` ctx
