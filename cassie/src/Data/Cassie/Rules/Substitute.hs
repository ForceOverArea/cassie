{- |
Module      :  Substitute
Description :  Monad transformer stack and actions for substituting algebraic structures into another algebra
Copyright   :  (c) Grant Christiansen
License     :  MIT

Maintainer  :  christiansengrant18@gmail.com
Stability   :  [unstable] | experimental | provisional | stable | frozen
Portability :  portable

TODO: change this module's description
-}

{-# LANGUAGE Safe #-}
module Data.Cassie.Rules.Substitute 
    ( substitute
    , pureSubstitute
    , substituteFnArgs
    , SubstitutionError(..)
    ) where

import safe Control.Monad
import safe Data.Cassie.Structures

-- | This type is deprecated. It is only left in to prevent build 
--   issues until consuming code can migrate to pureSubstitute.
data SubstitutionError 
    = ImpossibleError
    deriving (Show, Eq, Ord)

data AlgCrumb mg u n
    = Plural    { kind    :: AlgStruct mg u n 
                , items   :: [(Int, AlgStruct mg u n)]
                , indices :: [Int]
                }
    | MagmaOp   { magKind :: mg
                , other   :: Maybe (AlgStruct mg u n)
                , hasLeft :: Bool
                }
    | UnaryOp   { unyKind :: u
                }
    | Negate
    | Invert
    deriving (Show, Eq)

-- | This function is deprecated. Use pureSubstitute instead.
substitute :: AlgebraicStructure mg u n 
    => Symbol 
    -> AlgStruct mg u n 
    -> AlgStruct mg u n 
    -> Either SubstitutionError (AlgStruct mg u n)
substitute target replacement source = pure $ pureSubstitute target replacement source

-- | Substitutes all instances of a given @target@ symbol with a 
--   given @replacement@ algebraic structure in a third structure.
--   This function is non-partial a  
pureSubstitute :: AlgebraicStructure mg u n 
    => Symbol 
    -> AlgStruct mg u n 
    -> AlgStruct mg u n 
    -> AlgStruct mg u n
pureSubstitute target replacement source = 
    let
        pureSub = pureSubstitute target replacement
    in case source of 
        Additive terms          -> Additive $ (pureSub) <$> terms
        Multiplicative factors  -> Multiplicative $ (pureSub) <$> factors
        N_ary name arguments    -> N_ary name $ (pureSub) <$> arguments 
        Magma op l r            -> Magma op (pureSub l) (pureSub r)
        Unary op x              -> Unary op $ pureSub x
        Negated x               -> Negated $ pureSub x
        Inverse x               -> Inverse $ pureSub x
        Nullary n               -> Nullary n
        Symbol s                -> if target == s then replacement else source
    
substituteFnArgs :: AlgebraicStructure mg u n 
    => AlgStruct mg u n 
    -> [Symbol] 
    -> ([AlgStruct mg u n] -> Either SubstitutionError (AlgStruct mg u n))
substituteFnArgs impl args = 
    let 
        foldFunc source (tgt, replacement) = substitute tgt replacement source 
        subArgs = foldM foldFunc impl
    in subArgs . zip args
