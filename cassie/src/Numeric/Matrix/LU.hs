{-# LANGUAGE Trustworthy #-}
module Numeric.Matrix.LU 
    ( lupDecompose
    , lupDecomposeTol
    , lupDet
    , lupDetInv
    , lupDetTol
    , lupDetInvTol
    , lupInv
    , lupInvTol
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Control.Monad.Except
import safe Control.Monad.ST
import safe Control.Monad.Trans
import safe Data.STRef
import Data.Vector (iforM_)
import qualified Data.Vector.Unboxed.Mutable as MV
import safe Numeric.Matrix.Internal

type LUPDecomp s = ExceptT LUPDecomposeError (ST s)

data LUPDecomposeError = NotSquare | Degenerate deriving (Eq, Ord)

data LUP s a = LUP { edgeLen     :: Int
                   , lu          :: MV.MVector s a
                   , p           :: MV.MVector s a
                   , nRowChanges :: STRef s Int
                   }

instance Show LUPDecomposeError where
    show NotSquare  = "the given matrix is not square"
    show Degenerate = "the given matrix is degenerate"

defaultTol :: Fractional a => a
defaultTol = (fromInteger 1) / (fromInteger $ 10 ^ (12 :: Integer))

lupDecompose :: (Fractional a, Ord a, MV.Unbox a, Show a) => Matrix a -> Either LUPDecomposeError (Matrix a, Matrix a)
lupDecompose = lupDecomposeTol defaultTol

lupDet :: (Fractional a, Ord a, MV.Unbox a, Show a) => Matrix a -> Either LUPDecomposeError a
lupDet = lupDetTol defaultTol

lupInv :: (Fractional a, Ord a, MV.Unbox a, Show a) => Matrix a -> Either LUPDecomposeError (Matrix a)
lupInv = lupInvTol defaultTol

lupDetInv :: (Fractional a, Ord a, MV.Unbox a, Show a) => Matrix a -> Either LUPDecomposeError (a, Matrix a)
lupDetInv = lupDetInvTol defaultTol

lupDecomposeTol :: (Fractional a, Ord a, MV.Unbox a, Show a) => a -> Matrix a -> Either LUPDecomposeError (Matrix a, Matrix a)
lupDecomposeTol t a = 
    let
        f = runKleisli $ join (***) (Kleisli $ fromMVector (cols a))
    in runST $ runExceptT $ lupDecomposeMain t a >>= f . (lu &&& p)

lupDetTol :: (Fractional a, Ord a, MV.Unbox a, Show a) => a -> Matrix a -> Either LUPDecomposeError a
lupDetTol t a = runST $ runExceptT $ lupDecomposeMain t a >>= lupDetMain 

lupInvTol :: (Fractional a, Ord a, MV.Unbox a, Show a) => a -> Matrix a -> Either LUPDecomposeError (Matrix a)
lupInvTol t a = runST $ runExceptT $ lupDecomposeMain t a >>= lupInvMain 

lupDetInvTol :: (Fractional a, Ord a, MV.Unbox a, Show a) => a -> Matrix a -> Either LUPDecomposeError (a, Matrix a)
lupDetInvTol t a = runST $ runExceptT $ lupDecomposeMain t a >>= runKleisli (Kleisli lupDetMain &&& Kleisli lupInvMain)

lupDecomposeMain :: (Fractional a, Ord a, MV.Unbox a, Show a) => a -> Matrix a -> LUPDecomp s (LUP s a)
lupDecomposeMain tol a = 
    let
        _N = cols a
        n = _N - 1
        nElems = rows a * cols a

        f i | i == 0 || i `quot` _N == i `rem` _N = 1
            | otherwise = 0 
    in do
        when (rows a /= cols a) $ throwError NotSquare

        lup <- lift $ LUP _N <$> toMVector a 
                             <*> MV.generate nElems f
                             <*> newSTRef _N
                          
        forM_ [0..n] $ \i -> do
            maxA <- pivot i lup

            when (maxA < tol) $ throwError Degenerate

            forM_ [i + 1..n] $ \j -> do
                let a' = lu lup
                
                flip (MV.modifyM a') (_N -! (j, i))
                    $ \x -> (x /) <$> (a' !- _N $ (i, i))

                forM_ [i + 1..n] $ \k -> do
                    flip (MV.modifyM a') (_N -! (j, k))
                        $ \x -> do
                            prod <- (*) <$> (a' !- _N $ (j, i))
                                        <*> (a' !- _N $ (i, k))
                            pure $ x - prod   
        pure lup
        
lupDetMain :: (Fractional a, MV.Unbox a) => LUP s a -> LUPDecomp s a
lupDetMain lup = lift $ do
    let a = lu lup
    let _N = edgeLen lup
    let n = _N - 1
    rowChanges <- readSTRef $ nRowChanges lup
    det <- newSTRef =<< (a !- _N $ (0, 0))

    forM_ [1..n] $ \i -> do
        x <- a !- _N $ (i, i)
        modifySTRef det (* x)

    if (rowChanges - _N) `rem` 2 == 0 then
        readSTRef det
    else
        negate <$> readSTRef det

lupInvMain :: (Fractional a, MV.Unbox a) => LUP s a -> LUPDecomp s (Matrix a)
lupInvMain lup = 
    let
        a = lu lup
        ia = p lup
        _N = edgeLen lup
        n = _N - 1 
    in (fromMVector _N =<<) . lift $ do
        forM_ [0..n] $ \j -> do
            forM_ [0..n] $ \i -> do
                forM_ [0..i - 1] $ \k -> do
                    flip (MV.modifyM ia) (_N -! (i, j)) 
                        $ \x -> do
                            prod <- (*) <$> ( a !- _N $ (i, k)) 
                                        <*> (ia !- _N $ (k, j))
                            pure $ x - prod

            forM_ (reverse [0..n]) $ \i -> do
                forM_ [i + 1..n] $ \k -> do
                    flip (MV.modifyM ia) (_N -! (i, j)) 
                        $ \x -> do
                            prod <- (*) <$> ( a !- _N $ (i, k)) 
                                        <*> (ia !- _N $ (k, j))
                            pure $ x - prod
                
                flip (MV.modifyM ia) (_N -! (i, j)) 
                    $ \x -> (x /) <$> (a !- _N $ (i, i))
        pure ia 

pivot :: (Fractional a, Ord a, MV.Unbox a, Show a) => Int -> LUP s a -> LUPDecomp s a
pivot col lup = 
    let
        a = lu lup
        ia = p lup
        _N = edgeLen lup
        n = _N - 1
        rcs = nRowChanges lup

        findMax (iMax, maxA) i = do
            absA <- fmap abs $ a !- _N $ (i, col)
            if absA > maxA then
                pure (i, absA)
            else
                pure (iMax, maxA)
    in do
        (iMax, absA) <- foldM findMax (0, 0) [col..n]
        when (iMax /= col) $ do
            lift $ modifySTRef rcs (+ 1)
            forM_ [0..n] $ \j -> do
                MV.swap  a (_N -! (col, j)) (_N -! (iMax, j))
                MV.swap ia (_N -! (col, j)) (_N -! (iMax, j))
        pure absA

-- | Alias for @flattenIdx@.
(-!) :: Int -> MatIdx -> Int
n -! ij = flattenIdx n ij

-- | Alias for @Data.Vector.Unboxed.Mutable.read@ that incorporates the functionality of @(-!)@ with it.
--   (The @-@ always points towards the edge length of the matrix.)
(!-) :: (MV.PrimMonad m, MV.Unbox a) => MV.MVector (MV.PrimState m) a -> Int -> MatIdx -> m a
mv !- n = MV.read mv . (n -!)

toMVector :: (MV.Unbox a, Monad m, MV.PrimMonad m) => Matrix a -> m (MV.MVector (MV.PrimState m) a)
toMVector a = do
    ma <- MV.new (rows a * cols a)
    iforM_ (flatten a) $ MV.write ma
    pure ma

fromMVector :: MV.Unbox a => Int -> MV.MVector s a -> LUPDecomp s (Matrix a)
fromMVector nCols mv = 
    let
        prependToListM xs = pure . (:xs)
    in fromList nCols . reverse <$> MV.foldM prependToListM [] mv 

-- | Maps from a 2D @Matrix@ index to a @Data.Vector@ (or @MVector@) index.
flattenIdx :: Int -> MatIdx -> Int
flattenIdx _N (i, j) = _N * i + j
