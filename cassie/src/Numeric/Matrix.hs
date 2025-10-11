{-# LANGUAGE Trustworthy #-}
module Numeric.Matrix 
    ( (!)
    , size
    , toMatrix
    , toMatrix'
    , Matrix(rows, cols, flatten)
    ) where

import safe Control.Arrow
import qualified Data.Vector as V

data Matrix a = Matrix { cols    :: Int 
                       , rows    :: Int
                       , flatten :: V.Vector a
                       } 
                       deriving (Eq, Ord)

instance Functor Matrix where
    -- | Maps a given unary function over the elements of a matrix
    fmap f (Matrix c r elems) = Matrix c r $ f <$> elems

instance Num a => Num (Matrix a) where
    -- | Partial element-wise addition between matrices. See also: @matrixSum@
    a + b = maybe 
        (error "cannot add matrices of different shapes")
        id
        $ matrixSum a b

    -- | Partial matrix product. See also: @matrixProd@
    a * b = maybe 
        (error "cannot multiply matrices: columns of left matrix must equal rows of right matrix") 
        id 
        $ matrixProd a b

    -- | Element-wise absolute value 
    abs a = abs <$> a

    -- | Element-wise signum
    signum a = signum <$> a

    -- | Element-wise negation
    negate a = negate <$> a

    -- | Creates a 1x1 matrix containing the given integer
    fromInteger = toMatrix 1 . V.singleton . fromInteger

-- | Non-partial matrix sum
matrixSum :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
matrixSum a b =
    let 
        ra = rows a
        rb = rows b
        ca = cols a
        cb = cols b
    in if ra == rb && ca == cb then
        Just (Matrix ra ca $ V.zipWith (+) (flatten a) (flatten b))
    else
        Nothing
            
-- | Non-partial matrix product
matrixProd :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
matrixProd a b = 
    let 
        m = rows a
        n = cols b
        indices = (\x y -> (x, y)) <$> [0..m] <*> [0..n]
        vectors = ((`row` a) *** (`column` b)) <$> indices
    in if rows b == cols a then
        Just (toMatrix' n $ uncurry dot <$> vectors)
    else 
        Nothing

dot :: Num a => V.Vector a -> V.Vector a -> a 
dot a b = V.sum $ V.zipWith (*) a b

toMatrix :: Int -> V.Vector a -> Matrix a
toMatrix n v = Matrix n (V.length v) v

toMatrix' :: Int -> [a] -> Matrix a
toMatrix' n = toMatrix n . V.fromList

(!) :: Matrix a -> (Int, Int) -> a
m ! (i, j) = (flatten m) V.! (cols m * i + j)

size :: Matrix a -> Int
size = V.length . flatten

column :: Int -> Matrix a -> V.Vector a
column i m = (\j -> m ! (i, j)) <$> V.fromList [0..rows m]

row :: Int -> Matrix a -> V.Vector a
row j m = (\i -> m ! (i, j)) <$> V.fromList [0..cols m]
