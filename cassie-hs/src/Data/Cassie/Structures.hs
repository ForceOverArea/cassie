{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cassie.Structures
    ( (~?)
    , isolateLeftOperand
    , isolateRightOperand
    , AlgebraicStructure
    , AlgStruct(..)
    , CancelMagma(..)
    , CancelUnary(..)
    , Context
    , ComplexAlgStruct
    , ComplexCtx
    , ComplexEqn
    , ComplexMagma(..)
    , ComplexUnary(..)
    , CtxItem(..)
    , Equation(..)
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
import safe Data.Cassie.Structures.Magmas
import safe Data.Cassie.Structures.Instances.Real
import safe Data.Cassie.Structures.UnarySystems
import safe Data.Cassie.Structures.Internal


class ( MagmaMock m n
      , CancelMagma m
      , UnaryMock u n
      , CancelUnary u
      , Renderable m
      , Renderable u
      , Renderable n
      , Num n
      , Fractional n
      , Eq m
      , Eq u
      , Eq n
      ) => AlgebraicStructure m u n 

instance AlgebraicStructure RealMagma RealUnary Double 

instance ( Num n
         , Floating n
         , Fractional n
         , Renderable n
         , Eq n
         ) => AlgebraicStructure ComplexMagma ComplexUnary n
