{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cassie.Structures.Instances.Complex
    ( ComplexAlgStruct
    , ComplexCtx
    , ComplexEqn
    , ComplexMagma(..)
    , ComplexUnary(..)
    ) where

import safe Control.Arrow
import safe Control.Monad
import safe Data.Complex (Complex)
import safe Data.Cassie.Structures.Internal
import safe Data.Cassie.Structures.Magmas
import safe Data.Cassie.Structures.UnarySystems

type ComplexAlgStruct = AlgStruct ComplexMagma ComplexUnary (Complex Double)

type ComplexEqn = Equation ComplexMagma ComplexUnary (Complex Double)

type ComplexCtx = Context ComplexMagma ComplexUnary (Complex Double)

newtype ComplexMagma = ComplexMagma ExpnMagma deriving (Show, Eq, Ord)

instance Floating a => MagmaMock ComplexMagma a where
    evalMagma (ComplexMagma cm) = evalMagma cm

instance CancelMagma ComplexMagma where
    lCancel (ComplexMagma cm) = join (+++) ComplexMagma <$> lCancel cm

    rCancel (ComplexMagma cm) = join (+++) ComplexMagma <$> rCancel cm

instance ShowMagma ComplexMagma where
    showMagma (ComplexMagma cm) = showMagma cm

newtype ComplexUnary = ComplexUnary TrigUnary deriving (Show, Eq, Ord)

instance Floating a => UnaryMock ComplexUnary a where
    evalUnary (ComplexUnary cu) = evalUnary cu

instance CancelUnary ComplexUnary where
    cancel (ComplexUnary cu) = ComplexUnary <$> cancel cu

instance ShowUnary ComplexUnary where
    showUnary (ComplexUnary cu) = showUnary cu
