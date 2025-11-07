{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cassie.Structures.Magmas 
    ( isolateLeftOperand
    , isolateRightOperand
    , CancelMagma(..)
    , ExpnMagma(..)
    , MagmaMock(..)
    , ShowMagma(..)
    , XorMagma(..)
    ) where

import safe Data.Cassie.Structures.Internal

class MagmaMock m n where
    -- | Maps a marker representing a magma operation to its appropriate
    --   binary operation. 
    evalMagma :: m -> (n -> n -> n)

-- | Typeclass for values that represent a set of binary operations that form 
--   a magma along with elements of type @n@.
class CancelMagma m where
    -- | Read as "cancel the left operand out"
    --
    --   Given a magma operation and a left operand, this function yields a
    --   token that indicates what partially applied binary operation isolates 
    --   the right-hand operand in a given structure.
    --  
    --   Example: 
    --
    --   in the equation @2^x = 8@
    -- 
    --   where @Expn@ is a magma token type representing exponentiation (@^@) and 
    --   @Logm@ is a magma token type representing logarithms of an arbitrary base 
    --   (@log<x>(y)@)
    -- 
    --   @lCancel Expn@ should yield @Left Logm@ 
    --
    --   because @log<2>(2^x) = log<2>(8) = x@
    --
    --   i.e. because left-applied logarithms can cancel left-applied exponents
    lCancel :: m -> Maybe (Either m m)
    
    -- | Read as "cancel the left operand out"
    --   
    --   Given a magma operation and a left operand, this function yields a
    --   token that indicates what partially applied binary operation isolates 
    --   the right-hand operand in a given structure.
    --  
    --   Example: 
    --
    --   in the equation @x^3 = 8@
    -- 
    --   where @Expn@ is a magma token type representing exponentiation (@^@) and 
    --   @Root@ is a magma token type representing roots of an arbitrary radical 
    --   (@root<x>(y)@)
    -- 
    --   @rCancel Expn@ should yield @Left Root@ 
    --
    --   because @root<3>(x^3) = root<3>(8) = x@
    --
    --   i.e. because left-applied roots can cancel right-applied exponents
    rCancel :: m -> Maybe (Either m m)

class Show m => ShowMagma m where
    showMagma :: Show a => m -> a -> a -> String
    showMagma x l r = show l ++ show x ++ show r

data ExpnMagma = Expn | Logm | Root deriving (Show, Eq, Ord)

data XorMagma = Xor deriving (Show, Eq, Ord)

isolateRightOperand :: CancelMagma mg => mg -> AlgStruct mg u n -> Maybe (AlgStruct mg u n -> AlgStruct mg u n)
isolateRightOperand op l = do
    cancellativeOp <- lCancel op
    pure $ case cancellativeOp of
        Left op'  -> Magma op' l
        Right op' -> \x -> Magma op' x l

isolateLeftOperand :: CancelMagma mg => mg -> AlgStruct mg u n -> Maybe (AlgStruct mg u n -> AlgStruct mg u n)
isolateLeftOperand op r = do
    cancellativeOp <- rCancel op
    pure $ case cancellativeOp of 
        Left op'  -> Magma op' r
        Right op' -> \x -> Magma op' x r

instance (Floating a) => MagmaMock ExpnMagma a where
    evalMagma Expn = (**)
    evalMagma Logm = logBase
    evalMagma Root = flip (**) . (1.0 /)

instance CancelMagma ExpnMagma where
    lCancel mOp = 
        Just $ case mOp of
            Expn -> Left Logm
            Logm -> Left Expn
            Root -> Right Expn

    rCancel mOp = 
        Just $ case mOp of 
            Expn -> Left Root
            Logm -> Right Root
            Root -> Left Logm

instance ShowMagma ExpnMagma where
    showMagma Expn = \x y -> show x ++ "^" ++ show y
    showMagma Logm = \x y -> "log<" ++ show x ++ ">(" ++ show y ++ ")"
    showMagma Root = \x y -> "root<" ++ show x ++ ">(" ++ show y ++ ")"

instance Integral a => MagmaMock XorMagma a where 
    evalMagma Xor = (^)

instance CancelMagma XorMagma where
    lCancel = Just . Left
    
    rCancel = Just . Right

instance ShowMagma XorMagma where
    showMagma Xor = \x y -> show x ++ " ^ " ++ show y