{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cassie.Structures
    ( (~?)
    , isolateLeftOperand
    , isolateRightOperand
    , showAlgStruct
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
    , Symbol
    , UnaryMock(..)
    ) where

import safe Data.Cassie.Structures.Instances.Complex
import safe Data.Cassie.Structures.Instances.Real
import safe Data.Cassie.Structures.Internal
import safe Data.Cassie.Structures.Magmas
import safe Data.Cassie.Structures.ShowStructure
import safe Data.Cassie.Structures.UnarySystems
import safe Data.Complex

class ( MagmaMock m n
      , CancelMagma m
      , UnaryMock u n
      , CancelUnary u
      , ShowMagma m
      , ShowUnary u
      , Show n
      , Num n
      , Fractional n
      , Eq m
      , Eq u
      , Eq n
      ) => AlgebraicStructure m u n 

data Equation m u n = Equation { lhs :: AlgStruct m u n
                               , rhs :: AlgStruct m u n
                               }
                               deriving (Eq, Ord)

type ComplexEqn = Equation ComplexMagma ComplexUnary (Complex Double)

type RealEqn = Equation RealMagma RealUnary Double

instance (ShowMagma m, ShowUnary u, Show n, Num n) => Show (Equation m u n) where
    show (Equation l r) = showAlgStruct l ++ " = " ++ showAlgStruct r

instance AlgebraicStructure RealMagma RealUnary Double 

instance ( Num n
         , Floating n
         , Fractional n
         , Show n
         , Eq n
         ) => AlgebraicStructure ComplexMagma ComplexUnary n
