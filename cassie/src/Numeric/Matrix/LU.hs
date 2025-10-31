{-# LANGUAGE Safe #-}
module Numeric.Matrix.LU
    ( lupDecompose
    , lupDet
    , lupDetInv
    , lupInv
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Except
import safe Control.Monad.Identity
import safe Control.Monad.State
import safe Data.List as List
import safe Numeric.Matrix.Internal

type LUPDecompT a m = StateT (LUP a) (ExceptT LUPDecomposeError m)

data LUP a = LUP { lu :: Matrix a
                 , p  :: Matrix a
                 , permutationCount :: Int
                 } 
                 deriving Show

data LUPDecomposeError = NotSquare | Degenerate deriving (Eq, Ord)

instance Show LUPDecomposeError where
    show NotSquare  = "the given matrix is not square"
    show Degenerate = "the given matrix is degenerate"

lupDecompose :: (Fractional a, Ord a) => Matrix a -> Either LUPDecomposeError (Matrix a, Matrix a)
lupDecompose a = runIdentity 
               . runExceptT 
               $ evalStateT 
                    (lupDecompose' >> gets (lu &&& p))
                    (initState a)

lupDet :: (Fractional a, Ord a) => Matrix a -> Either LUPDecomposeError a
lupDet a = runIdentity 
         . runExceptT 
         $ evalStateT 
            (lupDecompose' >> lupDet') 
            (initState a)

lupInv :: (Fractional a, Ord a) => Matrix a -> Either LUPDecomposeError (Matrix a)
lupInv a = runIdentity 
         . runExceptT 
         $ evalStateT 
            (lupDecompose' >> lupInv' >> (gets $ p)) 
            (initState a)

lupDetInv :: (Fractional a, Ord a) => Matrix a -> Either LUPDecomposeError (a, Matrix a)
lupDetInv a = 
    let 
        f = Kleisli . const $ lupDet'
        g = Kleisli . const $ lupInv' >> (gets $ p)
        lupDetInv' = runKleisli (f &&& g) ()
    in do 
        runIdentity 
            . runExceptT 
            $ evalStateT 
                (lupDecompose' >> lupDetInv')
                (initState a)

lupDecompose' :: (Fractional a, Ord a, Monad m) => LUPDecompT a m ()
lupDecompose' = do
    n <- gets $ (+ (- 1)) . cols . lu  
    (\j -> pivot j >> reduceRows j) `mapM_` [0..n]

lupDet' :: (Num a, Monad m) => LUPDecompT a m a
lupDet' = do
    (a, permCount) <- gets $ (lu &&& permutationCount) 
    let det = product $ (a !) <$> join zip [0..cols a - 1]
    if permCount - cols a `rem` 2 == 0 then
        return det
    else 
        return $ -det

lupInv' :: (Fractional a, Ord a, Monad m) => LUPDecompT a m ()
lupInv' = do
    n <- gets $ (+ (- 1)) . cols . lu 
    (\i -> solveForY i >> solveForX i) `mapM_` [0..n]

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

        swapRows' i1 i2 a = 
            let 
                swapElem (i', j)
                    | i' == i2  = a ! (i1, j)
                    | i' == i1  = a ! (i2, j)
                    | otherwise = a ! (i', j)
            in mapIndices swapElem a
    in do
        (LUP lu' p' permutationCount') <- get
        imax <- pivotRow lu'
        when (i /= imax) 
            . modify
            . const 
            $ LUP { lu = swapRows' i imax lu'
                  , p  = swapRows' i imax p'
                  , permutationCount = permutationCount' + 1
                  }

-- | Reduces the rows of the LU matrix for a given column.
reduceRows :: (Fractional a, Monad m) => Int -> LUPDecompT a m ()
reduceRows i = 
    let 
        reduceIdx col a (i', j) 
            | col >= i'  = a ! (i', j) -- ignores rows that have already been reduced
            | col == j  = pivotA     
            | col < j   = a ! (i', j) - pivotA * a ! (col, j)
            | otherwise = a ! (i', j)
            where
                pivotA = a ! (i', col) / a ! (col, col)
    in do
        (a, lup) <- gets $ (lu &&& id) 
        modify . const 
               $ LUP { lu = mapIndices (reduceIdx i a) a
                     , p  = p lup
                     , permutationCount = permutationCount lup
                     }

solveForY :: (Num a, Monad m) => Int -> LUPDecompT a m ()
solveForY j = 
    let 
        lowerTriIndices n = 
            let
                lowerTriCols i = ((,) i) <$> [0..i - 1]
            in lowerTriCols <$> [0..n]
    in do
        n <- gets $ (+ (-1)) . cols . lu 
        mapM_ (modifyRow j) $ lowerTriIndices n

solveForX :: (Fractional a, Monad m) => Int -> LUPDecompT a m ()
solveForX j = 
    let
        upperTriIndices n = 
            let 
                upperTriCols i = ((,) i) <$> [i + 1..n]
            in upperTriCols <$> reverse [0..n]

        tagRows rows' = 
            let 
                f x y = [Right y, Left x]
            in concat $ zipWith f [0..] rows'

        g = Kleisli $ endOfRow j
        h = Kleisli $ modifyRow j

    in do
        n <- gets $ (+ (-1)) . cols . lu
        mapM_ (runKleisli $ g +++ h) . tagRows $ upperTriIndices n 

modifyRow :: (Num a, Monad m) => Int -> [MatIdx] -> LUPDecompT a m ()
modifyRow j rowIdxs = 
    let 
        reduceIdx a ia (i, k) 
            | (i, j) `elem` rowIdxs = ia ! (i, j) - a ! (i, k) * ia ! (k, j)
            | otherwise             = ia ! (i, j)
    in do
        (a, ia) <- gets $ lu &&& p
        modifyP . mapIndices $ reduceIdx a ia 

endOfRow :: (Fractional a, Monad m) => Int -> Int -> LUPDecompT a m ()
endOfRow j i = 
    let 
        reduceIdx a ia (i', j')
            | (i', j') == (i, j) = ia ! (i, j) / a ! (i, i)
            | otherwise          = ia ! (i', j')
    in do
        (a, ia) <- gets $ lu &&& p
        modifyP . mapIndices $ reduceIdx a ia

modifyP :: Monad m => (Matrix a -> Matrix a) -> LUPDecompT a m ()
modifyP f = do 
    (LUP lu' p' permCount) <- get
    modify . const $ LUP lu' (f p') permCount

initState :: Num a => Matrix a -> LUP a
initState a = 
    let 
        edgeLen = rows a
    in LUP { lu = a
           , p  = ident edgeLen
           , permutationCount = edgeLen
           }
