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
    ) where

import safe Data.Cassie.Structures.Internal

class MagmaMock m n where
    -- | Maps a marker representing a magma operation to its appropriate
    --   binary operation. 
    evalMagma :: m -> (n -> n -> n)

-- | Typeclass for values that represent a set of binary operations that form 
--   a magma along with elements of type @n@.
class CancelMagma m where    
    -- | Given a magma operation and a left operand, this function yields a
    --   function that performs the inverse operation on an algebraic structure.
    lCancel :: m -> Maybe (Either m m)
    
    -- | Given a magma operation and a right operand, this function yields a
    --   function that performs the inverse operation on an algebraic structure.
    rCancel :: m -> Maybe (Either m m)

class Show m => ShowMagma m where
    showMagma :: Show a => m -> (a -> a -> String)
    showMagma x = \l r -> show l ++ show x ++ show r

data ExpnMagma = Expn | Logm | Root deriving (Show, Eq, Ord)

isolateLeftOperand :: CancelMagma m => m -> AlgStruct m u n -> Maybe (AlgStruct m u n -> AlgStruct m u n)
isolateLeftOperand op l = do
    cancellativeOp <- lCancel op
    return $ case cancellativeOp of
        Left op'  -> Magma op' l
        Right op' -> \x -> Magma op' x l

isolateRightOperand :: CancelMagma m => m -> AlgStruct m u n -> Maybe (AlgStruct m u n -> AlgStruct m u n)
isolateRightOperand op r = do
    cancellativeOp <- rCancel op
    return $ case cancellativeOp of 
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
    showMagma Expn = \x y -> show x ++ " ^ " ++ show y
    showMagma Logm = \x y -> "log<" ++ show x ++ ">(" ++ show y ++ ")"
    showMagma Root = \x y -> "root<" ++ show x ++ ">(" ++ show y ++ ")"