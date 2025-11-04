{-# LANGUAGE Trustworthy #-}
module Numeric.Matrix.LU 
    ( lupDecompose
    , lupDet
    , lupDetInv
    , lupInv
    ) where

import safe Control.Monad
import safe Control.Monad.Except
import safe Control.Monad.ST
import safe Control.Monad.Trans
import safe Data.STRef
import qualified Data.Vector.Unboxed.Mutable as MV
import safe Numeric.Matrix.Internal

type LUPDecomp s = ExceptT LUPDecomposeError (ST s)

data LUPDecomposeError = NotSquare | Degenerate | NotInvertible deriving (Eq, Ord)

data LUP s a = LUP { lu :: MV.MVector s a
                   , _p  :: MV.MVector s a
                   , _nRowChanges :: STRef s Int
                   }

instance Show LUPDecomposeError where
    show NotSquare  = "the given matrix is not square"
    show Degenerate = "the given matrix is degenerate"
    show NotInvertible = "the given matrix has no inverse"

lupDecompose :: (Fractional a, Ord a, MV.Unbox a)
             => Matrix a 
             -> Either LUPDecomposeError (Matrix a)
lupDecompose a = 
    if rows a == cols a then
        runST $ runExceptT 
              $ lupDecomposeMain a 
              >>= fromMVector (cols a) . lu
    else 
        Left NotSquare

lupDet :: Fractional a 
       => Matrix a 
       -> Either LUPDecomposeError a
lupDet _a = error "TODO"

lupInv :: Fractional a 
       => Matrix a 
       -> Either LUPDecomposeError (Matrix a)
lupInv _a = error "TODO"

lupDetInv :: (Fractional a, MV.Unbox a)
          => Matrix a 
          -> Either LUPDecomposeError (a, Matrix a)
lupDetInv _a = error "TODO"

lupDecomposeMain :: (Fractional a, Ord a, MV.Unbox a)
                 => Matrix a 
                 -> LUPDecomp s (LUP s a)
lupDecomposeMain a = 
    let
        n = cols a - 1
        _N = cols a
        nElems = rows a * cols a

        f i | i == 0 || i `quot` nElems == i `rem` nElems = 1
            | otherwise = 0 
    in do
        lup <- lift $ LUP <$> MV.new nElems 
                          <*> MV.generate nElems f
                          <*> newSTRef 0
                          
        forM_ [0..n] $ \i -> do
            _maxA <- pivot i n lup
            
            -- when (maxA < 1E-12) $ throwError Degenerate -- FIXME: parametrize this magic number

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
        
pivot :: (Fractional a, Ord a, MV.Unbox a)
      => Int 
      -> Int
      -> LUP s a 
      -> LUPDecomp s a
pivot col n lup = 
    let
        _N = (n + 1)

        findMax (iMax, maxA) i = do
            absA <- fmap abs $ MV.read (lu lup) $ (_N -! (i, col))
            if absA > maxA then
                pure (i, absA)
            else 
                pure (iMax, maxA)
    in do
        (iMax, absA) <- foldM findMax (0, 0) [0..n]
        when (iMax /= col) $ 
            forM_ [0..n] $ \j -> do
                MV.swap (lu lup) (_N -! (col, j)) (_N -! (iMax, j))
        pure absA

-- | Alias for @flattenIdx@.
(-!) :: Int -> MatIdx -> Int
n -! ij = flattenIdx n ij

-- | Alias for @Data.Vector.Unboxed.Mutable.read@ that incorporates the functionality of @(-!)@ with it.
--   (The @-@ always points towards the edge length of the matrix.)
(!-) :: (MV.PrimMonad m, MV.Unbox a) => MV.MVector (MV.PrimState m) a -> Int -> MatIdx -> m a
mv !- n = MV.read mv . (n -!)

fromMVector :: MV.Unbox a => Int -> MV.MVector s a -> LUPDecomp s (Matrix a)
fromMVector nCols mv = 
    let
        prependToListM xs = pure . (:xs)
    in fromList nCols <$> MV.foldM prependToListM [] mv 

-- | Maps from a 2D @Matrix@ index to a @Data.Vector@ (or @MVector@) index.
flattenIdx :: Int -> MatIdx -> Int
flattenIdx edgeLen (i, j) = edgeLen * i + j

-- -- | Maps from a @Data.Vector@ (or @MVector@) index to a 2D @Matrix@ index.
-- unflattenIdx :: Int -> Int -> MatIdx
-- unflattenIdx _ 0 = (0, 0)
-- unflattenIdx edgeLen k = (`quot` edgeLen) &&& (`rem` edgeLen) $ k