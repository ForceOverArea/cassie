{-# LANGUAGE Trustworthy #-}
module Numeric.Matrix.Internal
    ( (!)
    , (Numeric.Matrix.Internal.!?)
    , augment
    , column
    , column'
    , cross
    , dot
    , dot'
    , fromList
    , ident
    , mapIndices
    , matrixProd
    , matrixSum
    , matrixTranspose
    , row
    , row'
    , size
    , toMatrix
    , toMatrix'
    , Matrix(..)
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Data.List
import safe Data.Maybe
import qualified Data.Vector as V

type MatIdx = (Int, Int)

data Matrix a = Matrix { cols    :: Int 
                       , rows    :: Int
                       , flatten :: V.Vector a
                       } 
                       deriving (Eq, Ord)

instance Show a => Show (Matrix a) where
    show (Matrix 0 _ _) = "[]"
    show (Matrix _ 0 _) = "[]"
    show a = 
        let 
            mtrxRows = V.toList . (`row'` a) <$> [0..rows a - 1]
            formatRow = intercalate ", " . map show
        in ('[' :) 
            . (++ "]") 
            . intercalate "; " 
            . map formatRow 
            $ mtrxRows

instance Functor Matrix where
    -- | Maps a given unary function over the elements of a matrix
    fmap f (Matrix c r elems) = Matrix c r $ f <$> elems

instance Num a => Num (Matrix a) where
    -- | Partial element-wise addition between matrices. See also: @matrixSum@
    a + b = 
        maybe 
            (error "cannot add matrices of different shapes")
            id
            $ matrixSum a b

    -- | Partial matrix product. See also: @matrixProd@
    a * b = 
        maybe 
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
    fromInteger = toMatrix' 1 . V.singleton . fromInteger

ident :: Num a => Int -> Matrix a
ident n = 
    let
        lasti = n ^ (2 :: Int) - 1

        f i | i == 0 || i `quot` n == i `rem` n = 1
            | otherwise = 0
    in fromList n $ f <$> [0..lasti]

-- | Non-partial matrix sum
matrixSum :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
matrixSum a b =
    let 
        ra = rows a
        rb = rows b
        ca = cols a
        cb = cols b
    in if ra == rb && ca == cb then
        Just . Matrix ra ca $ V.zipWith (+) (flatten a) (flatten b)
    else
        Nothing
            
-- | Non-partial matrix product
matrixProd :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
matrixProd a b = 
    let 
        m = rows a
        n = cols b
        indices = (,) <$> [0..m - 1] <*> [0..n - 1]
        vectors = ((`row'` a) *** (`column'` b)) <$> indices
    in if rows b == cols a then
        Just . fromList n $ uncurry dot' <$> vectors
    else 
        Nothing

matrixTranspose :: Matrix a -> Matrix a
matrixTranspose (Matrix 0 b c) = (Matrix b 0 c)
matrixTranspose (Matrix a 0 c) = (Matrix 0 a c)
matrixTranspose a = 
    let 
        rowsA = toMatrix' 1 . (`row'` a) <$> [0..rows a - 1]
        concatCols = uncurry $ foldM augment
    in fromJust $ uncons rowsA >>= concatCols

-- | Create an augment matrix
augment :: Matrix a -> Matrix a -> Maybe (Matrix a)
a `augment` b = 
    let
        m = rows a 
        n = cols a + cols b
        vectors = ((`row'` a) &&& (`row'` b)) <$> [0..m - 1]
        concat'd = mconcat $ uncurry (<>) <$> vectors
    in if m == rows b then
        Just . toMatrix' n $ concat'd
    else
        Nothing

-- | Type-safe dot product
dot :: Num a => V.Vector a -> V.Vector a -> Maybe a 
a `dot` b = 
    if V.length a == V.length b then
        Just $ a `dot'` b
    else 
        Nothing

-- | Unsafe dot product - does not multiply all pairs if vectors are different lengths
dot' :: Num a => V.Vector a -> V.Vector a -> a 
a `dot'` b = V.sum $ V.zipWith (*) a b

-- | Non-partial vector cross product
cross :: Num a => V.Vector a -> V.Vector a -> Maybe (V.Vector a)
a `cross` b = 
    if V.length a /= 3 || V.length b /= 3 then
        Nothing
    else
        Just $ V.fromList [ (a V.! 1) * (b V.! 2) - (a V.! 2) * (b V.! 1)
                          , (a V.! 2) * (b V.! 0) - (a V.! 0) * (b V.! 2)
                          , (a V.! 0) * (b V.! 1) - (a V.! 1) * (b V.! 0)
                          ]

-- | Allows one to apply a function @f@ to a subset of a matrix 
--   indicated by a list of indices. This yields a new matrix 
--   by iterating over all the original elements once.
mapIndices :: (MatIdx -> a) -> Matrix a -> Matrix a
mapIndices f a = 
    let 
        n = rows a * cols a - 1

        unflattenIdx 0 = (0, 0)
        unflattenIdx k = (`quot` cols a) &&& (`rem` cols a) $ k
        
    in fromList (cols a) $ (f . unflattenIdx) <$> [0..n]

-- | Create row'-rank @Matrix a@ with @n@ columns from a @Data.Vector.Vector a@
toMatrix :: Int -> V.Vector a -> Maybe (Matrix a)
toMatrix n v 
    | n == 0 && V.length v == 0 = Just $ Matrix 0 0 v
    | n == 0                    = Nothing
    | V.length v `rem` n == 0   = Just $ Matrix n (V.length v `quot` n) v
    | otherwise                 = Nothing

-- | Partial matrix construction
toMatrix' :: Int -> V.Vector a -> Matrix a
toMatrix' n v = 
    maybe 
        (error "cannot create row'-rank matrix from a list not evenly divisible by the number of columns")
        id
        $ toMatrix n v

-- | Create row'-rank @Matrix a@ with @n@ columns from a @[a]@
fromList :: Int -> [a] -> Matrix a
fromList n = toMatrix' n . V.fromList

-- | Non-partial matrix indexing
(!?) :: Matrix a -> MatIdx -> Maybe a
a !? (i, j) = 
    if i < rows a && j < cols a then
        Just $ (flatten a) V.! (cols a * i + j)
    else
        Nothing 
        
-- | Partial matrix indexing
(!) :: Matrix a -> MatIdx -> a
a ! (i, j) = 
    maybe
        (error $ "index out of bounds: '" ++ show (i, j) ++ "'")
        id
        $ a Numeric.Matrix.Internal.!? (i, j)

size :: Matrix a -> Int
size = V.length . flatten

-- | Non-partial column selection
column :: Int -> Matrix a -> Maybe (V.Vector a)
column j a = 
    if j < cols a then
        Just $ column' j a
    else 
        Nothing

-- | Non-partial row selection
row :: Int -> Matrix a -> Maybe (V.Vector a)
row i a = 
    if i < rows a then
        Just $ row' i a
    else 
        Nothing

-- | Partial column selection
column' :: Int -> Matrix a -> V.Vector a
column' j a = (\i -> a ! (i, j)) <$> V.fromList [0..rows a - 1]

-- | Partial row selection
row' :: Int -> Matrix a -> V.Vector a
row' i a = (\j -> a ! (i, j)) <$> V.fromList [0..cols a - 1]
