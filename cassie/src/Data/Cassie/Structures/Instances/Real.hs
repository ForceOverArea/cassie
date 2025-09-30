{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cassie.Structures.Instances.Real
    ( RealAlgStruct
    , RealMagma(..)
    , RealUnary(..)
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Data.Cassie.Structures.Internal
import safe Data.Cassie.Structures.Magmas
import safe Data.Cassie.Structures.Polynomial
import safe Data.Cassie.Structures.UnarySystems
import safe Data.Cassie.Utils

type RealAlgStruct = AlgStruct RealMagma RealUnary Double

newtype RealMagma = RealMagma ExpnMagma deriving (Show, Eq, Ord)

newtype RealUnary = RealUnary TrigUnary deriving (Show, Eq, Ord)

instance MagmaMock RealMagma Double where
    evalMagma (RealMagma rm) = evalMagma rm

instance CancelMagma RealMagma where
    lCancel (RealMagma rm) = join (+++) RealMagma <$> lCancel rm

    rCancel (RealMagma rm) = join (+++) RealMagma <$> rCancel rm

instance ShowMagma RealMagma where
    showMagma (RealMagma rm) = showMagma rm

instance UnaryMock RealUnary Double where
    evalUnary (RealUnary ru) = evalUnary ru

instance Polynomial (AlgStruct RealMagma RealUnary Double) where
    degree sym structure = 
        case structure of
            Additive terms             -> maximum <$> mapM (degree sym) terms 
            Multiplicative factors     -> sum <$> mapM (degree sym) factors
            Negated neg                -> degree sym neg
            Nullary _                  -> Just 0
            Symbol x                   -> if x == sym then Just 1 else Just 0
            Magma (RealMagma Expn) x (Nullary y) 
                -> do 
                    yi <- realInt y 
                    xi <- degree sym x 
                    return $ yi * xi
            _ -> Nothing

instance CancelUnary RealUnary where
    cancel (RealUnary ru) = RealUnary <$> cancel ru

instance ShowUnary RealUnary where
    showUnary (RealUnary ru) = showUnary ru