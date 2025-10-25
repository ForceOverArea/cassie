{-# LANGUAGE Trustworthy #-}
module Numeric.Matrix.LU
    ( lupDecompose
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Except
import safe Control.Monad.Identity
import safe Control.Monad.RWS
import safe Data.List as List
import qualified Data.Vector as V 
import safe Numeric.Matrix.Internal

type LUPDecompT a m = RWST Int () (LUP a, Int) (ExceptT LUPDecomposeError m)

data LUP a = LUP { lu :: Matrix a
                 , p  :: [Int]
                 , permutationCount :: Int
                 } 
                 deriving Show

data LUPDecomposeError = NotSquare | Degenerate deriving (Eq, Ord)

instance Show LUPDecomposeError where
    show NotSquare  = "the given matrix is not square"
    show Degenerate = "the given matrix is degenerate"

lupDecompose :: (Fractional a, Ord a) => Matrix a -> Either LUPDecomposeError (LUP a)
lupDecompose a = 
    let
        edgeLen = rows a

        initState = LUP { lu = a
                        , p  = [0..edgeLen - 1]
                        , permutationCount = 0
                        }

    in runIdentity . runExceptT $ fst . fst <$> execRWST lupDecompose' edgeLen (initState, 0)

lupDecompose' :: (Fractional a, Ord a, Monad m) => LUPDecompT a m ()
lupDecompose' = do
    col <- gets snd
    edgeLen <- ask
    if col + 1 >= edgeLen then
        return ()
    else do
        pivot col
        modifyRows col

modifyRows :: (Fractional a, Monad m) => Int -> LUPDecompT a m ()
modifyRows i = do
    edgeLen <- ask
    lup <- gets fst
    let idxs = (,) <$> [i + 1..edgeLen - 1] <*> [0..edgeLen - 1]
    let lu' = fromList edgeLen $ map (reduceIdx (lu lup) i) idxs
    modify . first 
           . const 
           $ LUP { lu = lu' 
                 , p  = p lup
                 , permutationCount = permutationCount lup
                 }

-- | Pivots if the matrix needs to be pivoted to reduce the @i@th column.
pivot :: (Num a, Ord a, Monad m) => Int -> LUPDecompT a m ()
pivot i = 
    let 
        -- Determine which row should be the pivot:
        pivotRow i' a = 
            let 
                absA k = abs $ a ! (k, i')
                compare' = curry $ uncurry compare . join (***) absA
                imax = maximumBy compare' [0..(length $ flatten a) - 1]
            in pure imax
    in do
        lu'  <- gets $ lu . fst
        imax <- pivotRow i lu'
        when (i /= imax) $ swapRows i imax

swapRows :: (Num a, Ord a, Monad m) => Int -> Int -> LUPDecompT a m ()
swapRows i j = do
    (LUP lu' p' permutationCount') <- gets fst
    modify . first 
           . const 
           $ LUP { lu = fromRows . (i >~ j) $ toRows lu'
                 , p  = (i >~ j) p'
                 , permutationCount = permutationCount' + 1
                 }

reduceIdx :: Fractional a => Matrix a -> Int -> (Int, Int) -> a 
reduceIdx a col (i, j)
    | (i <= col) || (j /= col) =  a ! (i, j)
    | (j == col)               = (a ! (i, j)) / (a ! (i, i))
    | otherwise                = (a ! (i, j)) - (a ! (i, col)) * (a ! (col, j))

(>~) :: Int -> Int -> [a] -> [a]
i >~ j = 
    \x -> let 
        f = mapIndex i . const $ x !! j
        g = mapIndex j . const $ x !! i
    in f $ g x

mapIndex :: Int -> (a -> a) -> [a] -> [a]
mapIndex i f xs = 
    let
        g (j, x)
            | j == i    = f x
            | otherwise = x
    in g <$> [0..] `zip` xs

fromRows :: Num a => [V.Vector a] -> Matrix a
fromRows [] = toMatrix' 0 mempty
fromRows (x:xs) = 
    let 
        nCols = V.length x
        contiguous = foldl' (<>) x xs
    in toMatrix' nCols contiguous

toRows :: Num a => Matrix a -> [V.Vector a]
toRows (Matrix r c elems) 
    | r <= 0    = []
    | otherwise = V.take c elems : (toRows (Matrix (r - 1) c $ V.drop c elems))
