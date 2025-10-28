{-# LANGUAGE Safe #-}
module Numeric.Matrix.LU
    ( lupDecompose
    , lupDet
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Except
import safe Control.Monad.Identity
import safe Control.Monad.State
import safe Data.List as List
import safe Numeric.Matrix.Internal

type LUPDecompT a m = StateT (LUP a, Int) (ExceptT LUPDecomposeError m)

data LUP a = LUP { lu :: Matrix a
                 , p  :: Matrix a
                 , permutationCount :: Int
                 } 
                 deriving Show

data LUPDecomposeError = NotSquare | Degenerate deriving (Eq, Ord)

instance Show LUPDecomposeError where
    show NotSquare  = "the given matrix is not square"
    show Degenerate = "the given matrix is degenerate"

lupDecompose :: (Fractional a, Ord a, Show a) => Matrix a -> Either LUPDecomposeError (Matrix a, Matrix a)
lupDecompose a = 
    let
        edgeLen = rows a

        initState = LUP { lu = a
                        , p  = ident edgeLen
                        , permutationCount = edgeLen
                        }
    in runIdentity 
        . runExceptT 
        $ evalStateT 
            (lupDecompose' >> gets ((lu &&& p) . fst))
            (initState, 0)

lupDet :: (Fractional a, Ord a, Show a) => Matrix a -> Either LUPDecomposeError a
lupDet a = 
    let
        edgeLen = rows a

        initState = LUP { lu = a
                        , p  = ident edgeLen
                        , permutationCount = edgeLen
                        }
    in runIdentity 
        . runExceptT 
        $ evalStateT 
            (lupDecompose' >> lupDet') 
            (initState, 0)

lupDecompose' :: (Fractional a, Ord a, Monad m, Show a) => LUPDecompT a m ()
lupDecompose' = do
    (edgeLen, col) <- gets $ cols . lu *** id
    if col < edgeLen then do
        pivot col
        reduceRows col
        modify $ second (+ 1) 
        lupDecompose'
    else 
        return ()

lupDet' :: (Num a, Monad m) => LUPDecompT a m a
lupDet' = do
    (a, permCount) <- gets $ (lu &&& permutationCount) . fst
    let det = product $ (a !) <$> join zip [0..cols a - 1]
    if permCount - cols a `rem` 2 == 0 then
        return det
    else 
        return $ -det

-- lupInv :: (Fractional a, Ord a, Monad m) => Matrix a -> LUPDecompT a m ()
-- lupInv = lupDecompose'

-- lupInv' :: (Fractional a, Ord a, Monad m) => Matrix a -> LUPDecompT a m ()
-- lupInv' ia = error "TODO"

-- | Pivots if the matrix needs to be pivoted to reduce the @i@th column.
pivot :: (Num a, Ord a, Monad m) => Int -> LUPDecompT a m ()
pivot i = 
    let 
        -- Determine which row should be the pivot:
        pivotRow a = 
            let 
                absA k = abs $ a ! (k, i)
                compare' = curry $ uncurry compare . join (***) absA
                imax = maximumBy compare' $ reverse [i..(cols a) - 1] -- reverse here prevents tie cases from screwing up results
            in pure imax
    in do
        lu'  <- gets $ lu . fst
        imax <- pivotRow lu'
        when (i /= imax) $ swapRows i imax

-- | Swaps the rows of the LU matrix
swapRows :: (Num a, Ord a, Monad m) => Int -> Int -> LUPDecompT a m ()
swapRows i j = 
    let 
        swapRows' i1 i2 a =
            let 
                f' (i', j')
                    | i' == i2  = a ! (i1, j')
                    | i' == i1  = a ! (i2, j')
                    | otherwise = a ! (i', j')
            in mapIndices f' a
    in do
        (LUP lu' p' permutationCount') <- gets fst
        modify . first 
               . const 
               $ LUP { lu = swapRows' i j lu'
                     , p  = swapRows' i j p'
                     , permutationCount = permutationCount' + 1
                     }

-- | Reduces the rows of the LU matrix for a given column.
reduceRows :: (Fractional a, Monad m, Show a) => Int -> LUPDecompT a m ()
reduceRows i = do
    (a, lup) <- gets $ (lu &&& id) . fst
    modify . first 
           . const 
           $ LUP { lu = mapIndices (reduceIdx i a) a
                 , p  = p lup
                 , permutationCount = permutationCount lup
                 }

-- | A piecewise function that reduces the element at a 
--   given index in a @Matrix a@ for reducing a given 
--   column.
reduceIdx :: (Fractional a) => Int -> Matrix a -> (Int, Int) -> a 
reduceIdx col a (i, j) 
    | col >= i  = a ! (i, j) -- ignores rows that have already been reduced
    | col == j  = pivotA     
    | col < j   = a ! (i, j) - pivotA * a ! (col, j)
    | otherwise = a ! (i, j)
    where
        pivotA = a ! (i, col) / a ! (col, col)
