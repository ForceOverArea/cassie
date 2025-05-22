{-# LANGUAGE Safe #-}
module Data.Cassie.Structures.Instances.Complex
    ( ComplexAlgStruct
    , ComplexCtx
    , ComplexEqn
    , ComplexMagma(..)
    , ComplexUnary(..)
    ) where

import safe Data.Complex (Complex)
import safe Data.Cassie.Structures.Internal
import safe Data.Cassie.Structures.Magmas
import safe Data.Cassie.Structures.UnarySystems

type ComplexAlgStruct = AlgStruct ComplexMagma ComplexUnary (Complex Double)

type ComplexEqn = Equation ComplexMagma ComplexUnary (Complex Double)

type ComplexCtx = Context ComplexMagma ComplexUnary (Complex Double)

newtype ComplexMagma = ComplexMagma ExpnMagma

newtype ComplexUnary = ComplexUnary TrigUnary
