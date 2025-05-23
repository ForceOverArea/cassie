{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cassie.Structures.Internal 
    ( (~?)
    , AlgStruct(..)
    , Context
    , CtxItem(..)
    , Equation(..)
    , Renderable(..)
    , Symbol
    ) where

import safe Control.Arrow 
import safe Control.Monad
import safe qualified Data.Map as Map
import safe Data.Complex
import safe Data.List as List
import safe qualified Data.List.NonEmpty as NE

class Renderable r where
    render :: r -> String

    needsParensAround :: r -> r -> Bool
    needsParensAround _ _ = False

type Symbol = String

type Context m u n = Map.Map String (CtxItem m u n)

data CtxItem m u n
    = Func [Symbol] (AlgStruct m u n)
    | Const (AlgStruct m u n)
    deriving (Show, Eq, Ord)

data Equation m u n = Equation { lhs :: AlgStruct m u n
                               , rhs :: AlgStruct m u n
                               }
                               deriving (Show, Eq, Ord)

-- | A heavily abstracted representation of a mathematical expression.
--   This record is designed to represent generalized algebraic 
--   structures over various sets of elements. It is, roughly speaking, a
--   mock of Haskell's @Num@ class.
-- 
--   TODO: add explanation of properties type params should satisfy
data AlgStruct m u n
    -- | Additive binary operation of elements
    = Additive       (NE.NonEmpty (AlgStruct m u n))

    -- | Multiplicative binary operation of elements
    | Multiplicative (NE.NonEmpty (AlgStruct m u n))
    
    -- | Unary negation of elements - essentially a mock of Haskell's 
    --   @Num a@ @negate@ function. This specifically represents the 
    --   additive inverse of the contained structure.
    | Negated        (AlgStruct m u n)

    -- | Unary inversion of elements - essentially a mock of Haskell's
    --   @Fractional a@ @recip@ function. This specifically represents 
    --   the multiplicative inverse of the contained structure.
    | Inverse        (AlgStruct m u n)

    -- | Magma over binary operation(s) indicated by a value of type @m@ 
    --   on elements of type @n@.
    | Magma          m (AlgStruct m u n) (AlgStruct m u n)

    -- | An operation of arbitrary arity, useful for working with user-defined functions
    | N_ary          String [AlgStruct m u n]

    -- | Unary system over operation(s) indicated by a value of type @u@
    --   on elements of type @n@.
    | Unary          u (AlgStruct m u n)

    -- | A raw element in the algebraic structure
    | Nullary        n

    -- | A symbol representing an arbitrary element in the algebraic structure.
    | Symbol         Symbol
    deriving (Show, Eq, Ord)


instance Num n => Num (AlgStruct m u n) where
    (Additive terms1) + (Additive terms2) = Additive $ terms1 <> terms2
    (Additive terms1) + x                 = Additive $ terms1 <> NE.fromList [x]
    x + (Additive terms2)                 = Additive $ NE.fromList [x] <> terms2
    x + y                                 = Additive $ NE.fromList [x, y]

    (Multiplicative fac1) * (Multiplicative fac2) = Multiplicative $ fac1 <> fac2
    (Multiplicative fac1) * x                     = Multiplicative $ fac1 <> NE.fromList [x]
    x                     * (Multiplicative fac2) = Multiplicative $ NE.fromList [x] <> fac2
    x * y                                         = Multiplicative $ NE.fromList [x, y]
    
    negate = Negated

    abs (Negated x) = x
    abs x           = x

    signum (Negated _) = Negated $ fromInteger 1
    signum _           = fromInteger 1

    fromInteger = Nullary . fromInteger

instance Fractional n => Fractional (AlgStruct m u n) where
    recip = Inverse

    fromRational = Nullary . fromRational

instance Renderable Double where
    render = show

instance Show a => Renderable (Complex a) where
    render = show

instance (Renderable m, Renderable u, Renderable n) => Renderable (CtxItem m u n) where
    render (Func args impl) = "(" ++ intercalate "," args ++ ") -> " ++ render impl
    render (Const impl) = render impl

instance (Renderable m, Renderable u, Renderable n) => Renderable (Equation m u n) where
    render eqn = (render $ lhs eqn) ++ " = " ++ (render $ rhs eqn)

instance (Renderable m, Renderable u, Renderable n) => Renderable (AlgStruct m u n) where
    render s = case s of
        Additive terms          -> renderTerms terms
        Multiplicative factors  -> renderFactors factors
        Negated neg             -> " - " ++ render neg
        Inverse inv             -> "1 / " ++ render inv
        Magma m l r             -> render l ++ render m ++ render r
        N_ary name args         -> name ++ "(" ++ intercalate "," (map render args)  ++ ")"
        Unary u x               -> render u ++ "(" ++ render x ++ ")"
        Nullary num             -> render num
        Symbol sym              -> sym
    
    Additive _ `needsParensAround` as =
        case as of
            Negated _   -> True
            Inverse _   -> True
            Magma _ _ _ -> True
            _           -> False

    Multiplicative _ `needsParensAround` as =
        case as of
            Additive _  -> True
            Negated _   -> True
            Magma _ _ _ -> True
            _           -> False
    
    Negated _ `needsParensAround` _ = True

    Inverse _ `needsParensAround` _ = True

    Magma _ _ _ `needsParensAround` as =
        case as of
            Additive _       -> True
            Multiplicative _ -> True
            Negated _        -> True
            Magma _ _ _      -> True
            _                -> False

    N_ary _ _ `needsParensAround` _ = False

    Unary _ _ `needsParensAround` _ = False

    Nullary _ `needsParensAround` _ = False
    
    Symbol _ `needsParensAround` _ = False

(~?) :: Symbol -> AlgStruct m u n -> Bool
sym ~? Additive terms           = any (sym ~?) terms
sym ~? Multiplicative factors   = any (sym ~?) factors
sym ~? Negated neg              = sym ~? neg
sym ~? Inverse inv              = sym ~? inv
sym ~? Magma _ l r              = sym ~? l || sym ~? r
sym ~? N_ary _ args             = any (sym ~?) args
sym ~? Unary _ x                = sym ~? x
_   ~? Nullary _                = False
sym ~? Symbol s                 = sym == s

renderTerms :: (Renderable m, Renderable u, Renderable n) => NE.NonEmpty (AlgStruct m u n) -> [Char]
renderTerms terms = 
    let
        renderTerm (Negated term) = render (Negated term)
        renderTerm term           = " + " ++ render term
    in case NE.uncons terms of
        (x, Just others) -> render x ++ concatMap renderTerm others
        (x, Nothing)     -> render x

renderFactors :: (Renderable m, Renderable u, Renderable n) => NE.NonEmpty (AlgStruct m u n) -> [Char]
renderFactors factors = 
    let 
        (denom, num) = renderBoth . partition inverted $ NE.toList factors

        renderBoth = join (***) (map render)

        inverted (Inverse _) = True
        inverted _           = False
    in "(" ++ intercalate "*" num ++ ") / (" ++ intercalate "*" denom ++ ")"

