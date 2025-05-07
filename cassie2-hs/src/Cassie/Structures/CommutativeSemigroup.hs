module Cassie.Structures.CommutativeSemigroup
    ( CommutativeSG(..)
    , Sum(..)
    , Product(..)
    , CommutativeSemigroup
    ) where

-- | A class with the same constraints as Semigroup
--   but should also fulfill the commutative property
--   i.e. @a <> b == b <> a@
class Semigroup a => CommutativeSemigroup a

data CommutativeSG a
    = Additive (Sum a)
    | Multiplicative (Product a)
    deriving (Show, Eq, Ord)

-- | A record type modelling a numerical sum
newtype Sum a = Sum [a] deriving (Show, Eq, Ord)

instance Semigroup (Sum a) where
    Sum x <> Sum y = Sum $ x <> y

instance CommutativeSemigroup (Sum a)

-- | A record type modelling a numerical product
newtype Product a = Product [a] deriving (Show, Eq, Ord)

instance Semigroup (Product a) where
    Product x <> Product y = Product $ x <> y

instance CommutativeSemigroup (Product a)
