{-# LANGUAGE Trustworthy #-}
module Data.Cassie.Structures.Instances.Linalg
    ( LinAlgElem(..)
    ) where

import safe Data.Cassie.Structures.Magmas
import safe Data.Cassie.Structures
-- import safe Data.List as List
import Numeric.LinearAlgebra.Data as NLA

-- type LinAlgStruct = AlgStruct LinAlgMagma LinAlgUnary LinAlgElem

-- | This type -up front- is a dumpster fire
data LinAlgElem
    = Mtrx (Matrix Double)
    | Vctr (Vector Double)
    | Sclr Double
    | DNE String -- Does Not Exist (with optional error message)
    deriving (Show, Eq)

newtype LinAlgMagma = LinAlgMagma ExpnMagma

data LinAlgUnary 
    = Trig TrigUnary
    | Transpose
    deriving (Show, Eq, Ord)

instance Num LinAlgElem where
    -- Addition
    Mtrx a + Mtrx b
        | cols a == cols b && rows a == rows b = Mtrx $ a + b 
        | otherwise = DNE "failed to add: matrices must have same size"
    
    Vctr a + Vctr b
        | size a == size b = Vctr $ a + b
        | otherwise = DNE "failed to add: vectors must have same length"

    Sclr a + Sclr b = Sclr $ a + b

    _ + _ = DNE "failed to add: addends must be of matching types"

    -- Multiplication
    -- matrix-matrix element-wise
    Mtrx a * Mtrx b 
        | cols a == cols b && rows a == rows b = Mtrx $ a * b
        | otherwise = DNE "failed to multiply: matrices must be of same size"

    -- vector-vector element-wise
    Vctr a * Vctr b 
        | size a == size b = Vctr $ a * b
        | otherwise = DNE "failed to multiply: vectors must be of same length"

    -- scalar-scalar
    Sclr a * Sclr b = Sclr $ a * b 

    -- scalar-vector element-wise both ways   
    Sclr a * Vctr b = Vctr $  (vector $ replicate (size b) a) * b 

    Vctr a * Sclr b = Vctr $ a * (vector $ replicate (size a) b)

    -- scalar-matrix element-wise both ways
    Sclr a * Mtrx b = 
        let
            (numRows, numCols) = size b
            matrixSize = numRows * numCols
        in Mtrx $ (matrix numCols $ replicate matrixSize a) * b 

    Mtrx a * Sclr b = 
        let
            (numRows, numCols) = size a
            matrixSize = numRows * numCols
        in Mtrx $ a * (matrix numCols $ replicate matrixSize b)

    -- other combinations don't make sense
    _ * _ = DNE "failed to multiply: factors must be of matching types or one must be scalar"

    negate (Mtrx a) = Mtrx $ negate a
    negate (Vctr a) = Vctr $ negate a
    negate (Sclr a) = Sclr $ negate a
    negate (DNE a) = DNE a

    abs (Mtrx a) = Mtrx $ abs a
    abs (Vctr a) = Vctr $ abs a
    abs (Sclr a) = Sclr $ abs a
    abs (DNE a) = DNE a

    signum (Mtrx a) = Mtrx $ signum a
    signum (Vctr a) = Vctr $ signum a
    signum (Sclr a) = Sclr $ signum a
    signum (DNE a) = DNE a

    fromInteger = Sclr . fromInteger

instance Fractional LinAlgElem where
    fromRational = Sclr . fromRational

    recip (Sclr a) = Sclr $ recip a

    recip (Vctr a) = Vctr $ recip a

    recip (Mtrx a) = Mtrx $ recip a

    recip (DNE a) = DNE a

-- instance Floating a => MagmaMock LinAlgMagma a where
--     evalMagma (LinAlgMagma lam) = 



