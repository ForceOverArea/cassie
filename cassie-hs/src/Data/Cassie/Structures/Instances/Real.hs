{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cassie.Structures.Instances.Real
    ( RealAlgStruct
    , RealCtx
    , RealEqn
    , RealMagma(..)
    , RealUnary(..)
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Data.Cassie.Structures.Internal
import safe Data.Cassie.Structures.Magmas
import safe Data.Cassie.Structures.UnarySystems

type RealAlgStruct = AlgStruct RealMagma RealUnary Double

type RealEqn = Equation RealMagma RealUnary Double

type RealCtx = Context RealMagma RealUnary Double

newtype RealMagma = RealMagma ExpnMagma deriving (Show, Eq, Ord)

instance MagmaMock RealMagma Double where
    evalMagma (RealMagma rm) = evalMagma rm

instance CancelMagma RealMagma where
    lCancel (RealMagma rm) = join (+++) RealMagma <$> lCancel rm

    rCancel (RealMagma rm) = join (+++) RealMagma <$> rCancel rm

newtype RealUnary = RealUnary TrigUnary deriving (Show, Eq, Ord)

instance UnaryMock RealUnary Double where
    evalUnary (RealUnary ru) = evalUnary ru

instance CancelUnary RealUnary where
    cancel (RealUnary ru) = RealUnary <$> cancel ru
