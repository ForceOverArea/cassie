{-
    Helper module for tweaking context before attempting a solution 
    or before exporting context from a locally solved module.
-}
{-# LANGUAGE Safe #-}
module Data.Cassie.Solver.CtxEval
    ( strictEvalCtx
    , strictEvalCtxT
    , captureFunctionDepsT
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Except
import safe Control.Monad.Identity
import safe Data.Cassie.Rules
import safe Data.Cassie.Solver.Internal
import safe Data.Cassie.Structures
import safe qualified Data.Map as Map

strictEvalCtxT :: (Monad m, AlgebraicStructure mg u n) => Context mg u n -> ExceptT (CassieError mg u n) m (Context mg u n)
strictEvalCtxT = strictEvalCtxHelper 0

strictEvalCtx :: AlgebraicStructure mg u n => Context mg u n -> Either (CassieError mg u n) (Context mg u n)
strictEvalCtx ctx = runIdentity . runExceptT $ strictEvalCtxHelper 0 ctx

-- | Ensures that the context being referenced by a solution is self-sufficient. 
--   If this step is not completed prior to attempting a solution, then the 
--   algorithm may return an error for context items who have an undetected
--   dependency on an undefined symbol. 
-- 
--   TODO: this function should be rewritten to evaluate consts as they are
--   needed according to the root value's dep tree.
strictEvalCtxHelper :: (Monad m, AlgebraicStructure mg u n) 
                    => Int 
                    -> Context mg u n 
                    -> ExceptT (CassieError mg u n) m (Context mg u n)
strictEvalCtxHelper count ctx = 
    let
        concreteCtx = Map.foldlWithKey addConcreteValueToCtx mempty ctx

        addConcreteValueToCtx ctxAcc sym expr = 
            if isConst expr && (not $ sym `Map.member` ctxAcc) then -- 
                case evaluate ctxAcc $ numeric expr of
                    Left _err -> ctxAcc -- ignore things we can't know yet
                    Right ans -> Map.insert sym (Known (Nullary ans) mempty) ctxAcc
            else if not $ isConst expr then
                Map.insert sym expr ctxAcc
            else -- no need to evaluate functions or values we already know
                ctxAcc

    in if count > Map.size ctx then
        throwError . ContextMissingFor $ Map.keys ctx
    else if Map.keys concreteCtx == Map.keys ctx then
        pure concreteCtx
    else
        strictEvalCtxHelper (count + 1) $ concreteCtx `Map.union` ctx

-- | Substitutes all function implementations in a given context to capture 
--   constants pulled in from imported solutions and locally evaluated context.
captureFunctionDepsT :: (Monad m, AlgebraicStructure mg u n) 
                    => Solution mg u n 
                    -> Context mg u n 
                    -> ExceptT (CassieError mg u n) m (Context mg u n)
captureFunctionDepsT importedSoln ctx = 
    let 
        -- | Captures values from context and subs them into local function implementations
        captureValuesFor f =
            let 
                args = arguments f
                impl = implementation f
                deps = dependencies f
                
                tryCaptureDependency impl' x = do
                    capture <- captureValue x
                    pure $ pureSubstitute x capture impl'

            in if isConst f then -- meaning not a function
                Right f
            else
                (\x -> Func args x mempty) 
                <$> (foldM tryCaptureDependency impl deps)

        -- | Pulls values from context or imported solutions by name (TODO: these errors can be more specific on what exactly the context is missing)
        captureValue val
            | val `Map.member` importedSoln = 
                case possVal $ importedSoln Map.! val of
                    Right x -> Right $ Nullary x
                    _ -> Left $ ContextMissingFor [val]
            | val `Map.member` ctx = 
                case ctx Map.! val of
                    Known (Nullary x) _ -> Right $ Nullary x
                    _ -> Left $ ContextMissingFor [val]
            | otherwise = Left $ ContextMissingFor [val]

    in (throwError ||| pure) $ mapM captureValuesFor ctx 