{-# LANGUAGE Safe #-}
module Data.Cassie.Solver 
    ( solveCassieSystem
    , solveCassieSystemT
    , solveConstrainedMain
    , CassieError(..)
    , EquationPool
    , Solution
    , SolutionItem(..)
    , Symbols
    ) where

import safe Control.Monad.Except
import safe Control.Monad.Identity
import safe Control.Monad.RWS
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Solver.CtxEval
import safe Data.Cassie.Solver.Internal
import safe Data.Cassie.Structures 

solveCassieSystem :: AlgebraicStructure mg u n
    => Context mg u n 
    -> Solution mg u n
    -> EquationPool mg u n
    -> Either (CassieError mg u n) (Solution mg u n, EquationPool mg u n)
solveCassieSystem ctx presolved equations = runIdentity 
    $ solveCassieSystemT ctx presolved equations

solveCassieSystemT :: (AlgebraicStructure mg u n, Monad m)
    => Context mg u n 
    -> Solution mg u n
    -> EquationPool mg u n
    -> m (Either (CassieError mg u n) (Solution mg u n, EquationPool mg u n))
solveCassieSystemT ctx presolved equations = do 
    ans <- runExceptT $ do
        concreteCtx <- strictEvalCtx ctx 
        execRWST solveConstrainedMain concreteCtx (presolved, equations)
    pure $ fst <$> ans 
    -- Note: this may need to be a more complicated arrow if the system solver 
    --       picks up a @Writer@ context to log the order of solutions.
