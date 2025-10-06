{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cassie.Structures
    ( (~?)
    , getSyms
    , isolateLeftOperand
    , isolateRightOperand
    , showAlgStruct
    , AlgebraicStructure
    , AlgStruct(..)
    , BoolRing(..)
    , CancelMagma(..)
    , CancelUnary(..)
    , ComplexAlgStruct
    , ComplexEqn
    , ComplexMagma(..)
    , ComplexUnary(..)
    , Equation(..)
    , ExpnMagma(..)
    , MagmaMock(..)
    , Mixed(..)
    , MixedAlgStruct
    , MixedEqn
    , MixedMagma(..)
    , MixedUnary(..)
    , RealAlgStruct
    , RealEqn
    , RealMagma(..)
    , RealUnary(..)
    , ShowMagma(..)
    , ShowUnary(..)
    , Symbol
    , TrigUnary(..)
    , UnaryMock(..)
    ) where

import safe Data.Cassie.Structures.Instances.Boolean
import safe Data.Cassie.Structures.Instances.Complex
import safe Data.Cassie.Structures.Instances.Linalg
import safe Data.Cassie.Structures.Instances.Real
import safe Data.Cassie.Structures.Internal
import safe Data.Cassie.Structures.Magmas
import safe Data.Cassie.Structures.ShowStructure
import safe Data.Cassie.Structures.UnarySystems
import safe Data.Complex

class ( MagmaMock mg n
      , CancelMagma mg
      , UnaryMock u n
      , CancelUnary u
      , ShowMagma mg
      , ShowUnary u
      , Show n
      , Num n
      , Fractional n
      , Eq mg
      , Eq u
      , Eq n
      , Ord mg
      , Ord u
      , Ord n
      ) => AlgebraicStructure mg u n 

data Equation mg u n = Equation { lhs :: AlgStruct mg u n
                                , rhs :: AlgStruct mg u n
                                }
                                deriving (Eq, Ord)

type ComplexEqn = Equation ComplexMagma ComplexUnary (Complex Double)

type MixedEqn = Equation MixedMagma MixedUnary Mixed

type RealEqn = Equation RealMagma RealUnary Double

instance (ShowMagma mg, ShowUnary u, Show n, Num n) => Show (Equation mg u n) where
    show (Equation l r) = showAlgStruct l ++ " = " ++ showAlgStruct r

instance AlgebraicStructure RealMagma RealUnary Double 

instance ( Num n
         , Floating n
         , Fractional n
         , Show n
         , Eq n
         , Ord n
         ) => AlgebraicStructure ComplexMagma ComplexUnary n

instance AlgebraicStructure MixedMagma MixedUnary Mixed
