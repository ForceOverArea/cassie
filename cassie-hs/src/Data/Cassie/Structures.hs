{-# LANGUAGE Safe #-}
module Data.Cassie.Structures
    ( (~?)
    , AlgElement
    , AlgStruct(..)
    , Context
    , ComplexAlgStruct
    , ComplexCtx
    , ComplexEqn
    , ComplexMagma(..)
    , ComplexUnary(..)
    , CtxItem(..)
    , Equation
    , MagmaMock(..)
    , RealAlgStruct
    , RealCtx
    , RealEqn
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
import safe Data.Cassie.Structures.Internal ((~?), AlgElement, AlgStruct(..), Context, CtxItem(..), Equation, Renderable(..), Symbol)
