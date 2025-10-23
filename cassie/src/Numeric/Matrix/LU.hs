{-
    NOTE: many functions in this module are partial. They are
    meant to be fully encapsulated within the matrix reduction
    /inversion algorithms and are not intended for individual 
    use.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Trustworthy #-}
module Numeric.Matrix.LU
    ( incCol
    , luDecomp
    , luDecompInternal
    , luDet
    , luInv
    , pivot
    , reduceCol
    , toRows
    , LUP(..)
    , SizeData(..)
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Identity
import safe Control.Monad.RWS
import safe Data.List as List
import qualified Data.Vector as V
import safe Numeric.Matrix.Internal

type LUDecompT a m = RWST SizeData () (LUP a) m

type Rows a = [V.Vector a]

-- | Stateful data for LU decomposition algorithm
data LUP a = LUP { l     :: Rows a  -- `L` matrix mutated while decomposing `A`
                 , p     :: Rows a  -- `P` matrix formed from pivoting `A`
                 , u     :: Rows a  -- `U` matrix formed from decomposing `A`
                 , count :: Int     -- counter for number of row changes performed
                 , col   :: Int     -- index of the column to reduce
                 }
                 deriving (Show, Eq, Ord)

-- | Read-only data for LU decomposition algorithm
data SizeData = SizeData { nRows :: Int 
                         , nCols :: Int
                         }
                         deriving (Show, Eq, Ord)

data LUDecompError 
    = NotSquare
    deriving Show

luInv :: (Monad m, Fractional a, Ord a) => LUDecompT a m (Rows a)
luInv = do
    (_perm, _a) <- gets (p &&& u)
    return ([])
        
luDet :: (Monad m, Fractional a, Ord a) => LUDecompT a m a
luDet = do
    (a, rowChanges) <- gets (u &&& count)
    let det = foldl' (*) 1
            $ (\i -> a !! i V.! i) 
            <$> [0..length a - 1]
    if (rowChanges - length a) `rem` 2 == 0 then
        return det
    else 
        return (-det)

luDecomp :: (Fractional a, Ord a) => Matrix a -> Either LUDecompError (Matrix a, Matrix a, Matrix a)
luDecomp a = 
    if rows a /= cols a then
        Left NotSquare
    else let
        n = length $ toRows a
        
        initState = LUP { l = toRows $ ident n
                        , p = toRows $ ident n
                        , u = toRows a
                        , count = 0 
                        , col = 0
                        }

        sizedata = SizeData { nRows = rows a
                            , nCols = cols a
                            }

        result = fst . runIdentity $ execRWST luDecompInternal sizedata initState

        l' = toMatrix' n . foldl' (<>) mempty $ l result
        u' = toMatrix' n . foldl' (<>) mempty $ u result
        p' = toMatrix' n . foldl' (<>) mempty $ p result

    in return (l', u', p')

luDecompInternal :: (Monad m, Fractional a, Ord a) => LUDecompT a m ()
luDecompInternal = do
    n <- asks nCols
    i <- gets col
    if i < n then do
        pivot i
        reduceCol i
        incCol
        -- luDecompInternal
    else
        return ()

-- | reduces the @i@th column of @a@, pivoting as necessary
reduceCol :: (Fractional a, Ord a, Monad m) => Int -> LUDecompT a m ()
reduceCol i = 
    let 
        -- row operation formula for reducing lower left elements 
        f coef j = (i >+ j)     -- add ith row to jth row (2nd)
                 . (j >* coef)  -- scale jth row by coef  (1st)
    in do
        n <- (+ (-1)) . length <$> gets u
        coefs <- lij i `mapM` [i + 1..n]
        let rowOps = zipWith f coefs [i + 1..n]
        modify . uArr $ foldl' (.) id rowOps -- compose row ops and apply to u

-- | Computes and sets the value of 'Lij' in the lower matrix
lij :: (Fractional a, Monad m) => Int -> Int -> LUDecompT a m a
lij !i !j = do
    a <- gets u
    let coef = -1 * (a !! j V.! i) / (a !! i V.! i)
    modify . lArr $ setIndex i j coef
    return coef

-- | Pivots if the matrix needs to be pivoted to reduce the @i@th column.
pivot :: (Ord a, Monad m) => Int -> LUDecompT a m ()
pivot i = 
    let 
        -- Determine which row should be the pivot:
        pivotRow i' a = 
            let 
                absA k = a !! k V.! i'
                compare' = curry $ uncurry compare . join (***) absA
                imax = maximumBy compare' [0..length a - 1]
            in pure imax
    in do
        imax <- pivotRow i =<< gets u
        when (i /= imax) $ do
            modify $ uArr (i >~ imax) . pArr (i >~ imax)
            countRowChange

-- | Swap rows 
(>~) :: Int -> Int -> Rows a -> Rows a
i1 >~ i2 =  
    \x -> let 
        f = mapIndex i1 . const $ x !! i2
        g = mapIndex i2 . const $ x !! i1
    in f $ g x

-- | Add row @i1@ to row @i2@
(>+) :: Num a => Int -> Int -> Rows a -> Rows a
i1 >+ i2 = 
    \x -> let
        f = V.zipWith (+) $ x !! i1
    in mapIndex i2 f x

-- | Scale row @i@ by @s@
(>*) :: Num a => Int -> a -> Rows a -> Rows a
i >* s = 
    \x -> let 
        f = V.map (* s)
    in mapIndex i f x

lArr :: (Rows a -> Rows a) -> LUP a -> LUP a
lArr f (LUP l' u' p' count' col') = LUP (f l') u' p' count' col'

uArr :: (Rows a -> Rows a) -> LUP a -> LUP a
uArr f (LUP l' u' p' count' col') = LUP l' (f u') p' count' col'

pArr :: (Rows a -> Rows a) -> LUP a -> LUP a
pArr f (LUP l' u' p' count' col') = LUP l' u' (f p') count' col'

countRowChange :: Monad m => LUDecompT a m ()
countRowChange = do
    (LUP l' u' p' count' col') <- get
    put $ LUP l' u' p' (count' + 1) col'

incCol :: Monad m => LUDecompT a m ()
incCol = do
    (LUP l' u' p' count' col') <- get
    put $ LUP l' u' p' count' (col' + 1)

setIndex :: Int -> Int -> a -> Rows a -> Rows a
setIndex i j a = mapIndex i (mapIndexV j $ const a)

mapIndex :: Int -> (a -> a) -> [a] -> [a]
mapIndex i f xs = 
    let
        g (j, x)
            | j == i    = f x
            | otherwise = x
    in g <$> [0..] `zip` xs

mapIndexV :: Int -> (a -> a) -> V.Vector a -> V.Vector a
mapIndexV i f xs = 
    let
        g (j, x)
            | j == i    = f x
            | otherwise = x
    in g <$> V.fromList [0..] `V.zip` xs

toRows :: Num a => Matrix a -> Rows a
toRows (Matrix 0 _ _) = []
toRows (Matrix r c elems) = V.take c elems : (toRows (Matrix (r - 1) c $ V.drop c elems))
