{-# LANGUAGE Trustworthy #-}
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
import qualified Data.Vector as V 
import safe Numeric.Matrix.Internal

type LUPDecompT a m = StateT (LUP a, Int) (ExceptT LUPDecomposeError m)

data LUP a = LUP { lu :: Matrix a
                 , p  :: [Int]
                 , permutationCount :: Int
                 } 
                 deriving Show

data LUPDecomposeError = NotSquare | Degenerate deriving (Eq, Ord)

instance Show LUPDecomposeError where
    show NotSquare  = "the given matrix is not square"
    show Degenerate = "the given matrix is degenerate"

lupDecompose :: (Fractional a, Ord a) => Matrix a -> Either LUPDecomposeError (Matrix a)
lupDecompose a = 
    let
        edgeLen = rows a

        initState = LUP { lu = a
                        , p  = [0..edgeLen - 1]
                        , permutationCount = edgeLen
                        }
    in runIdentity 
        . runExceptT 
        $ evalStateT 
            (lupDecompose' >> gets (lu . fst))
            (initState, 0)

lupDet :: (Fractional a, Ord a) => Matrix a -> Either LUPDecomposeError a
lupDet a = 
    let
        edgeLen = rows a

        initState = LUP { lu = a
                        , p  = [0..edgeLen - 1]
                        , permutationCount = edgeLen
                        }
    in runIdentity 
        . runExceptT 
        $ evalStateT 
            (lupDecompose' >> lupDet') 
            (initState, 0)

lupDecompose' :: (Fractional a, Ord a, Monad m) => LUPDecompT a m ()
lupDecompose' = do
    col <- gets snd
    edgeLen <- gets $ cols . lu . fst
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
                imax = maximumBy compare' $ reverse [i..(cols a) - 1]
            in pure imax
    in do
        lu'  <- gets $ lu . fst
        imax <- pivotRow lu'
        when (i /= imax) $ swapRows i imax

swapRows :: (Num a, Ord a, Monad m) => Int -> Int -> LUPDecompT a m ()
swapRows i j = do
    (LUP lu' p' permutationCount') <- gets fst
    modify 
        . first 
        . const 
        $ LUP { lu = fromRows . (i >~ j) $ toRows lu'
              , p  = (i >~ j) p'
              , permutationCount = permutationCount' + 1
              }

reduceRows :: (Fractional a, Monad m) => Int -> LUPDecompT a m ()
reduceRows i = do
    (n, lup) <- gets $ ((+ (-1)) . cols . lu &&& id) . fst
    let idxs = (,) <$> [i + 1..n] <*> [0..n]
    let a = lu lup
    modify 
        . first 
        . const 
        $ LUP { lu = mapIndices idxs (reduceIdx i a) a
              , p  = p lup
              , permutationCount = permutationCount lup
              }

reduceIdx :: Fractional a => Int -> Matrix a -> (Int, Int) -> a 
reduceIdx col a (i, j) 
    | col == j  = pivotA
    | col < j   = a ! (i, j) - pivotA * a ! (col, j)
    | otherwise = a ! (i, j)
    where
        pivotA = a ! (i, col) / a ! (0, 0)
        -- log' = join $ trace ((show (i, j) ++) . show)

-- reduceRowsInv1 :: (Fractional a, Monad m) => Int -> Matrix a -> LUPDecompT a m ()
-- reduceRowsInv1 j ia = do
--     (n, lup) <- gets $ (cols . lu &&& id) . fst
--     -- NOTE: this is kinda magical-looking:
--     --       In the Wikipedia C implementation, we loop over k where k < i.
--     --       Here, first range is i, second range is k.
--     let idxs = filter (uncurry (>)) $ (,) <$> [0..n] <*> [0..n]
--     return ()

-- ia :: Num a => [Int] -> Matrix a
-- ia p' = 
--     let 
--         n = length p'
--         _1s = zipWith (,) [0..] p'
--     in mapIndices _1s (const $ fromInteger 1) 
--         . fromList n 
--         . replicate (n * n) 
--         $ fromInteger 0

-- reduceIdxInv :: Fractional a => Int -> Matrix a -> Matrix a -> (Int, Int) -> a 
-- reduceIdxInv col a ia (i, k)
--     | 

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
