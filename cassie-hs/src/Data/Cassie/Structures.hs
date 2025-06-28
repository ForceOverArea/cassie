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
    , CancelMagma(..)
    , CancelUnary(..)
    , ComplexAlgStruct
    , ComplexEqn
    , ComplexMagma(..)
    , ComplexUnary(..)
    , Equation(..)
    , MagmaMock(..)
    , RealAlgStruct
    , RealEqn
    , RealMagma(..)
    , RealUnary(..)
    , ShowMagma(..)
    , ShowUnary(..)
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
      ) => AlgebraicStructure mg u n 

data Equation mg u n = Equation { lhs :: AlgStruct mg u n
                               , rhs :: AlgStruct mg u n
                               }
                               deriving (Eq, Ord)

type ComplexEqn = Equation ComplexMagma ComplexUnary (Complex Double)

type RealEqn = Equation RealMagma RealUnary Double

instance (ShowMagma mg, ShowUnary u, Show n, Num n) => Show (Equation mg u n) where
    show (Equation l r) = showAlgStruct l ++ " = " ++ showAlgStruct r

instance AlgebraicStructure RealMagma RealUnary Double 

instance ( Num n
         , Floating n
         , Fractional n
         , Show n
         , Eq n
         ) => AlgebraicStructure ComplexMagma ComplexUnary n
