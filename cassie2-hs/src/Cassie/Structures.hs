module Cassie.Structures
    ( AlgStruct(..)
    ) where

import Cassie.Structures.CommutativeSemigroup (ComSG(..), Sum(Sum), Product(Product))
import Cassie.Structures.BinaryOps (BinaryOp(..), Difference(Difference))
import Cassie.Structures.Elements (UnaryOp(..))
import Cassie.Structures.Internal (Invertible(..))

data AlgStruct 
    = ComSG (CommutativeSG AlgStruct)
    | Bin   (BinaryOp      AlgStruct)
    | Un    (UnaryOp       AlgStruct)
    deriving (Show, Eq, Ord)

instance Invertible Sum where
    -- (^-) :: Sum a -> (a -> Bool) -> (a, a -> Difference a)
    Sum terms ^- predicate = 