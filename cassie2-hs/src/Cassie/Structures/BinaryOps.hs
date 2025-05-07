module Cassie.Structures.BinaryOps
    ( Difference(..)
    , Exponent(..)
    , Logarithm(..)
    , QuasiGroup(..)
    , Quotient(..)
    ) where

newtype Difference a = Difference (a, a) deriving (Show, Eq, Ord)

newtype Quotient a = Quotient (a, a) deriving (Show, Eq, Ord)

newtype Exponent a = Exponent (a, a) deriving (Show, Eq, Ord)

newtype Logarithm a = Logarithm (a, a) deriving (Show, Eq, Ord)

data BinaryOp a
    = Diff (Difference a)
    | Quot (Quotient a)
    | Exp (Exponent a)
    | Log (Logarithm a)
    deriving (Show, Eq, Ord)
