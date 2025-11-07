{-# LANGUAGE Safe #-}
module Data.Cassie.Structures.Instances.Boolean 
    ( BoolRing(..)
    ) where

{-
This module exists as a proof-of-concept (and a bit of a canary)
that the CASsie backend is element-type-agnostic. I.e. the same 
logic written into @Data.Cassie.Rules@ applies to this as to 
algebra with real numbers.

The applicable magma
-}

newtype BoolRing = BoolRing Bool deriving (Show, Eq, Ord)

instance Num BoolRing where
    BoolRing x + BoolRing y = BoolRing $ x || y

    BoolRing x * BoolRing y = BoolRing $ x && y

    abs x = x

    signum x = x

    negate (BoolRing x) = BoolRing $ not x

    fromInteger 0 = BoolRing False
    fromInteger _ = BoolRing True