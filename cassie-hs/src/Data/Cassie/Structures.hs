{-# LANGUAGE Safe #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Cassie.Structures 
    ( (~?)
    , show'
    , lhs
    , rhs
    , AlgebraicStruct(..)
    , Equation(..)
    , Symbol
    ) where

import safe Data.List

-- | A type alias for a symbol that may be nested inside an algebraic structure
type Symbol = String

-- | An equation represented by two algebraic structures whose values are deemed to be equivalent
newtype Equation = Equation (AlgebraicStruct, AlgebraicStruct)

instance Show Equation where
    show (Equation (lhs, rhs)) = show lhs ++ " = " ++ show rhs

-- | Represents a (possibly nested) 'schoolyard algebra' structure
data AlgebraicStruct
    -- | Represents the sum of an unknown number of terms
    = Sum [AlgebraicStruct]

    -- | Represents the difference of an unknown number of subtrahends
    | Difference [AlgebraicStruct]

    -- | Represents the product of an unknown number of factors 
    | Product [AlgebraicStruct]

    -- | Represents the quotient of a @dividend@ and a @divisor@
    | Quotient 
        { dividend :: AlgebraicStruct
        , divisor  :: AlgebraicStruct
        }

    -- | Represents a @base@ value raised to the power of @exponent@
    | Exponent
        { base :: AlgebraicStruct
        , expn :: AlgebraicStruct
        }

    -- | Represents the exponent required to achieve @log@ given @base@
    | Logarithm
        { base :: AlgebraicStruct
        , logm :: AlgebraicStruct
        }

    -- | Represents a function 
    | Function 
        { fname :: String
        , argv  :: [AlgebraicStruct]
        }

    -- | Represents a grouped set of algebraic structures contained within parenthesis
    | Group AlgebraicStruct

    -- | Represents a raw numeric value that cannot be reversed and contains no other algebraic structures
    | Value Double

    -- | Represents a raw value that cannot be reversed and contains no other algebraic structures
    | Symbol String
    deriving (Show, Eq, Ord)

show' :: AlgebraicStruct -> String
show' (Sum terms) = intercalate " + " $ map show' terms

show' (Difference subtrahends) = intercalate " - " $ map show' subtrahends

show' (Product factors) = intercalate " * " $ map show' factors

show' (Quotient sor dend) = show' sor ++ " / " ++ show dend

show' (Exponent b e) = show' b ++ " ^ " ++ show' e

show' (Logarithm b l) = "log " ++ show' b ++ "(" ++ show' l ++ ")"

show' (Function n a) = n ++ "(" ++ intercalate "," (map show' a) ++ ")"

show' (Group g) = "(" ++ show' g ++ ")"

show' (Value val) = show val

show' (Symbol sym) = sym

-- | A binary operation that reveals whether @sym@ is present as 
--   a raw symbol in the given @AlgebraicStruct@.
(~?) :: AlgebraicStruct -> Symbol -> Bool
Sum terms ~? sym = any (~? sym) terms

Difference subtrahends ~? sym = any (~? sym) subtrahends

Product factors ~? sym = any (~? sym) factors

Quotient s d ~? sym = any (~? sym) [s, d]

Exponent b e ~? sym = any (~? sym) [b, e]

Logarithm b l ~? sym = any (~? sym) [b, l]

Function _ a ~? sym = any (~? sym) a

Group g ~? sym = g ~? sym

Value _ ~? _ = False

Symbol x ~? sym = x == sym

lhs :: Equation -> AlgebraicStruct
lhs (Equation (lhs, _rhs)) = lhs

rhs :: Equation -> AlgebraicStruct
rhs (Equation (_lhs, rhs)) = rhs
