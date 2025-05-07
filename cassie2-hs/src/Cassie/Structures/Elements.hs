module Cassie.Structures.Elements 
    ( Group(..)
    , InverseTrigFunctions(..)
    , TrigFunctions(..)
    , UnaryOp(..)
    ) where

newtype Group a = Group a deriving (Show, Eq, Ord)

data TrigFunctions a 
    = Sin a
    | Cos a
    | Tan a
    deriving (Show, Eq, Ord)

data InverseTrigFunctions a
    = Arcsin a
    | Arccos a
    | Arctan a
    deriving (Show, Eq, Ord)

data UnaryOp a
    = Grp (Group a)
    | Trig (TrigFunctions a)
    | Arctrig (InverseTrigFunctions a)
    deriving (Show, Eq, Ord)
