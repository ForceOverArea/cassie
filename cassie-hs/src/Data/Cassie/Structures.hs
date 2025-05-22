{-# LANGUAGE Safe #-}
module Data.Cassie.Structures
    ( AlgStruct(..)
    , Context
    , ComplexAlgStruct
    , ComplexCtx
    , ComplexMagma(..)
    , ComplexUnary(..)
    , CtxItem(..)
    , Equation
    , MagmaMock(..)
    , RealAlgStruct
    , RealCtx
    , RealMagma(..)
    , RealUnary(..)
    , Renderable(..)
    , Symbol
    , UnaryMock(..)
    ) where

import safe Data.Cassie.Structures.Instances.Complex
import safe Data.Cassie.Structures.Magmas (MagmaMock(..))
import safe Data.Cassie.Structures.Instances.Real
import safe Data.Cassie.Structures.UnarySystems (UnaryMock(..))
import safe Data.Cassie.Structures.Internal (AlgStruct(..), Context, CtxItem(..), Equation, Renderable(..), Symbol)
