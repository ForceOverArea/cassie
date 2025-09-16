{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}
module Data.Cassie.Solver.System
    (
    ) where

import safe Control.Monad.Except
import safe Control.Monad.State
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Rules.Isolate
import safe Data.Cassie.Rules.Substitute
import safe Data.Cassie.Solver.Internal
import safe Data.Cassie.Structures
import safe Data.Cassie.Structures.Internal
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set

data CassieSystemState mg u n = CassieSystemState
    { equationPool :: EquationPool mg u n 
    , localCtx     :: Context mg u n -- ^ context local to solving the subsystem symbolically
    }
    deriving (Show, Eq, Ord)

data CassieSystemError mg u n 
    = SystemIsolationError (IsolateError mg u n)
    | SystemSubstitutionError SubstitutionError
    deriving (Show, Eq, Ord)

-- | A monad transformer for processes used in solving a subset of the equations in the global equation pool
type CassieSystemT mg u n m = StateT (CassieSystemState mg u n) (ExceptT (CassieSystemError mg u n) m)

-- | Given the necessary global information from the solver, 
--   returns context that can be @Data.Map.union@ed into the 
--   global context.
solveSubsystem :: Monad m 
    => EquationPool mg u n 
    -> Context mg u n 
    -> m (Either (CassieSystemError mg u n) (Context mg u n))
solveSubsystem globalEquationPool globalContext =
    getConstrainedSubsystem globalEquationPool >>= \case
        Left err -> pure $ Left err
        Right localEqPool -> runExceptT 
            $ evalStateT 
                solveSubsystemMain 
                (CassieSystemState localEqPool globalContext)

solveSubsystemMain :: Monad m => CassieSystemT mg u n m (Context mg u n)
solveSubsystemMain = do
    -- TODO
    return mempty

-- | Retrieves a subset of equations that form a constrained 
--   problem if one exists.
getConstrainedSubsystem :: Monad m
    => EquationPool mg u n 
    -> m (Either (CassieSystemError mg u n) (EquationPool mg u n))
getConstrainedSubsystem globalEquationPool = do
    -- TODO
    return $ Right mempty

-- | Finds a subset of the equation pool that forms an 'n' DOF 
--   subsystem of 'n' equations (if one exists)
getSubsystemUsing :: Monad m 
    => (Equation mg u n, Symbols) 
    -> EquationPool mg u n
    -> m (Maybe (EquationPool mg u n))
getSubsystemUsing (_eqn, syms) pool = 
    let 
        relatedEqns = filter (intersects syms . snd) pool
        numDof = Set.size . Set.unions $ map snd relatedEqns
    in if length relatedEqns == numDof then
        return $ Just relatedEqns
    else
        return Nothing

popEqn :: Monad m => CassieSystemT mg u n m (Maybe (Equation mg u n, Symbols))
popEqn = do
    gets equationPool >>= \case 
        (x:xs) -> do
            modify . modifyEquationPool $ const xs
            return $ Just x
        _ -> return Nothing

intersects :: Ord a => Set.Set a -> Set.Set a -> Bool
intersects x = (0 <) . Set.size . Set.intersection x

modifyEquationPool :: (EquationPool mg u n -> EquationPool mg u n) 
    -> CassieSystemState mg u n 
    -> CassieSystemState mg u n 
modifyEquationPool f (CassieSystemState a b) = CassieSystemState (f a) b

modifyLocalCtx :: (Context mg u n -> Context mg u n) 
    -> CassieSystemState mg u n 
    -> CassieSystemState mg u n 
modifyLocalCtx f (CassieSystemState a b) = CassieSystemState a (f b)
