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

import safe Control.Monad.Except (runExceptT)
import safe Control.Monad.Identity (runIdentity)
import safe Control.Monad.RWS (execRWST)
import safe Data.Cassie.Rules.Evaluate
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
    ans <- runExceptT $ execRWST solveConstrainedMain ctx (presolved, equations)
    return $ fst <$> ans 
    -- Note: this may need to be a more complicated arrow if the system solver 
    --       picks up a @Writer@ context to log the order of solutions.
