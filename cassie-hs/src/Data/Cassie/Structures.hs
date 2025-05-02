{-# LANGUAGE Safe #-}
module Data.Cassie.Structures 
    ( (~?)
    , getSymbol
    , leftHand
    , rightHand
    , showAlgStruct
    , AlgebraicStruct(..)
    , Equation(..)
    , Symbol
    ) where

import safe Data.List

-- | A type alias for a symbol that may be nested inside an algebraic structure
type Symbol = String

-- | An equation represented by two algebraic structures whose values are deemed to be equivalent
newtype Equation = Equation (AlgebraicStruct, AlgebraicStruct) deriving Eq

instance Show Equation where
    show (Equation (lhs, rhs)) = showAlgStruct lhs ++ " = " ++ showAlgStruct rhs

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

showAlgStruct :: AlgebraicStruct -> String
showAlgStruct (Sum terms) = intercalate " + " $ map showAlgStruct terms

showAlgStruct (Difference subtrahends) = intercalate " - " $ map showAlgStruct subtrahends

showAlgStruct (Product factors) = intercalate " * " $ map showAlgStruct factors

showAlgStruct (Quotient sor dend) = showAlgStruct sor ++ " / " ++ showAlgStruct dend

showAlgStruct (Exponent b e) = showAlgStruct b ++ " ^ " ++ showAlgStruct e

showAlgStruct (Logarithm b l) = "log<" ++ showAlgStruct b ++ ">(" ++ showAlgStruct l ++ ")"

showAlgStruct (Function n a) = n ++ "(" ++ intercalate "," (map showAlgStruct a) ++ ")"

showAlgStruct (Group g) = "(" ++ showAlgStruct g ++ ")"

showAlgStruct (Value val) = show val

showAlgStruct (Symbol sym) = sym

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

leftHand :: Equation -> AlgebraicStruct
leftHand (Equation (x, _)) = x

rightHand :: Equation -> AlgebraicStruct
rightHand (Equation (_, x)) = x

getSymbol :: AlgebraicStruct -> Symbol
getSymbol (Symbol x) = x
getSymbol _ = error "given structure was not a symbol"

