{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Cassie.Structures.Internal 
    ( (~?)
    , getSyms
    , AlgStruct(..)
    , Symbol
    ) where

import safe qualified Data.List.NonEmpty as NE
import safe qualified Data.Set as Set

type Symbol = String

-- | A heavily abstracted representation of a mathematical expression.
--   This record is designed to represent generalized algebraic 
--   structures over various sets of elements. It is, roughly speaking, a
--   mock of Haskell's @Num@ class.
-- 
--   TODO: add explanation of properties type params should satisfy
data AlgStruct mg u n
    -- | Additive binary operation of elements
    = Additive       (NE.NonEmpty (AlgStruct mg u n))

    -- | Multiplicative binary operation of elements
    | Multiplicative (NE.NonEmpty (AlgStruct mg u n))
    
    -- | Unary negation of elements - essentially a mock of Haskell's 
    --   @Num a@ @negate@ function. This specifically represents the 
    --   additive inverse of the contained structure.
    | Negated        (AlgStruct mg u n)

    -- | Unary inversion of elements - essentially a mock of Haskell's
    --   @Fractional a@ @recip@ function. This specifically represents 
    --   the multiplicative inverse of the contained structure.
    | Inverse        (AlgStruct mg u n)

    -- | Magma over binary operation(s) indicated by a value of type @m@ 
    --   on elements of type @n@.
    | Magma          mg (AlgStruct mg u n) (AlgStruct mg u n)

    -- | An operation of arbitrary arity, useful for working with user-defined functions
    | N_ary          String [AlgStruct mg u n]

    -- | Unary system over operation(s) indicated by a value of type @u@
    --   on elements of type @n@.
    | Unary          u (AlgStruct mg u n)

    -- | A raw element in the algebraic structure
    | Nullary        n

    -- | A symbol representing an arbitrary element in the algebraic structure.
    | Symbol         Symbol
    deriving (Show, Eq, Ord)

instance Num n => Num (AlgStruct mg u n) where
    (Additive terms1) + (Additive terms2) = Additive $ terms1 <> terms2
    (Additive terms1) + x                 = Additive $ terms1 <> NE.fromList [x]
    x + (Additive terms2)                 = Additive $ NE.fromList [x] <> terms2
    x + y                                 = Additive $ NE.fromList [x, y]

    (Multiplicative fac1) * (Multiplicative fac2) = Multiplicative $ fac1 <> fac2
    (Multiplicative fac1) * x                     = Multiplicative $ fac1 <> NE.fromList [x]
    x                     * (Multiplicative fac2) = Multiplicative $ NE.fromList [x] <> fac2
    x * y                                         = Multiplicative $ NE.fromList [x, y]
    
    negate (Negated x) = x
    negate x = Negated x

    abs (Negated x) = x
    abs x           = x

    signum (Negated _) = Negated $ fromInteger 1
    signum _           = fromInteger 1

    fromInteger = Nullary . fromInteger

instance Fractional n => Fractional (AlgStruct mg u n) where
    recip (Inverse x) = x
    recip x = Inverse x

    fromRational = Nullary . fromRational

-- | Indicates whether the given symbol is present in the given structure.
(~?) :: Symbol -> AlgStruct mg u n -> Bool
sym ~? Additive terms           = any (sym ~?) terms
sym ~? Multiplicative factors   = any (sym ~?) factors
sym ~? Negated neg              = sym ~? neg
sym ~? Inverse inv              = sym ~? inv
sym ~? Magma _ l r              = sym ~? l || sym ~? r
sym ~? N_ary _ args             = any (sym ~?) args
sym ~? Unary _ x                = sym ~? x
_   ~? Nullary _                = False
sym ~? Symbol s                 = sym == s

-- | Retrieves all the symbols present in a given algebraic structure. 
getSyms :: AlgStruct mg u n -> Set.Set Symbol
getSyms (Additive terms)         = Set.unions $ NE.map getSyms terms
getSyms (Multiplicative factors) = Set.unions $ NE.map getSyms factors
getSyms (Negated neg)            = getSyms neg
getSyms (Inverse inv)            = getSyms inv 
getSyms (Magma _ l r)            = getSyms l `Set.union` getSyms r
getSyms (N_ary _ args)           = Set.unions $ map getSyms args
getSyms (Unary _ x)              = getSyms x
getSyms (Nullary _)              = mempty
getSyms (Symbol s)               = Set.singleton s
