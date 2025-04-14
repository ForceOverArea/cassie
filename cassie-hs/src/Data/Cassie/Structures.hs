{-# LANGUAGE Safe #-}
module Data.Cassie.Structures 
    ( emap
    , AlgebraicStruct(..)
    , Equation(..)
    , Symbol
    ) where

import safe Prelude hiding (exp)
import safe Data.List (intercalate)

-- | A type alias for a symbol that may be nested inside an algebraic structure
type Symbol = String

-- | An equation represented by two algebraic structures whose values are deemed to be equivalent
newtype Equation = Equation (AlgebraicStruct, AlgebraicStruct)

instance Show Equation where
    show (Equation (lhs, rhs)) = show lhs ++ " = " ++ show rhs

emap :: ((AlgebraicStruct, AlgebraicStruct) -> (AlgebraicStruct, AlgebraicStruct)) -> Equation -> Equation
emap f (Equation sides) = Equation (f sides)

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
        , exp  :: AlgebraicStruct
        }

    -- | Represents the exponent required to achieve @log@ given @base@
    | Logarithm
        { base :: AlgebraicStruct
        , log  :: AlgebraicStruct
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
    deriving (Eq, Ord)

instance Show AlgebraicStruct where
    show (Sum terms) = intercalate " + " $ map show terms

    show (Difference subtrahends) = intercalate " - " $ map show subtrahends

    show (Product factors) = intercalate " * " $ map show factors

    show (Quotient sor dend) = show sor ++ " / " ++ show dend

    show (Exponent b e) = show b ++ " ^ " ++ show e

    show (Logarithm b l) = "log " ++ show b ++ "(" ++ show l ++ ")"

    show (Function n a) = n ++ "(" ++ intercalate "," (map show a) ++ ")"

    show (Group g) = "(" ++ show g ++ ")"

    show (Value val) = show val

    show (Symbol sym) = sym
