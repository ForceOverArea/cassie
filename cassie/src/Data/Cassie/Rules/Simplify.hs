{-# LANGUAGE Safe #-}
module Data.Cassie.Rules.Simplify 
    ( simplify
    ) where

import safe Control.Arrow
import safe Data.Either
import safe qualified Data.List.NonEmpty as NE
import safe qualified Data.Map as Map
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Structures

simplify :: AlgebraicStructure mg u n => AlgStruct mg u n -> AlgStruct mg u n
simplify src = 
    case src of 
        Additive ts -> Additive $ simplifyAddv ts
        _ -> error "ligma"

simplifyAddv :: (AlgebraicStructure mg u n) 
             => NE.NonEmpty (AlgStruct mg u n)
             -> NE.NonEmpty (AlgStruct mg u n)
simplifyAddv = groupTerms . cancelTerms

cancelTerms :: NE.NonEmpty (AlgStruct mg u n)
            -> NE.NonEmpty (AlgStruct mg u n)
cancelTerms = 
    let
        f =  NE.filter isRight . NE.map (evaluate Map.empty)
    in error "ligma"

-- | Simplifies a term in the context of being additive
groupTerms :: (AlgebraicStructure mg u n) 
           => NE.NonEmpty (AlgStruct mg u n) 
           -> NE.NonEmpty (AlgStruct mg u n)
groupTerms ts = 
    let 
        nOccurrences target = length . NE.filter (== target)
        
        occurrenceMap
            = uncurry NE.zip
            $ (NE.map (flip nOccurrences ts) . NE.nub) &&& id
            $ ts
        
        makeProduct 
            = Multiplicative 
            . (\(x, y) -> NE.fromList [x, y])
            . first (Nullary . fromIntegral)
    in
        NE.map makeProduct occurrenceMap