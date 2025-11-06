{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists #-}
module Data.Cassie.Rules.Isolate.PolySolve 
    ( {- consolidate -} commonFactors
    , factorOut
    , factorize
    , factors
    , PolySolveError
    ) where

import safe Control.Monad
import safe qualified Data.Set as Set
import safe qualified Data.List.NonEmpty as NE
import safe Data.Cassie.Structures (AlgebraicStructure)
import safe Data.Cassie.Structures.Internal ((~?), AlgStruct(..), Symbol)

data PolySolveError mg u n
    = FactorNotFound (AlgStruct mg u n) (AlgStruct mg u n)
    | NoCommonFactors
    | TargetNotAFactor
    deriving (Show, Eq, Ord)

-- consolidate :: AlgebraicStructure mg u n
--     => AlgStruct mg u n
--     -> Either (PolySolveError mg u n) (AlgStruct mg u n)
-- consolidate structure = 
--     let
        
--     in 


-- likeFactors :: AlgebraicStructure mg u n 
--     => AlgStruct mg u n
--     -> Set.Set AlgStruct mg u n
-- likeFactors structure =
--     case structure of  
--         Additive terms -> 

{-
    x +     x - 3  -> 2x - 3
2 * x + 3 * x - 5  -> 5x - 5
x ^ 2 + x - 3      -> x^2 + x - 3 
-}

-- | Attempts to remove arcs from a given algebraic structure by
--   reversing distributive multiplication of substructures 
--   containing @sym@ from @src@. This will always return a
--   @Multiplicative@ constructor value containing the product of
--   the substructure and any common factor of interest (i.e. containing 
--   @target@) found within.
factorize :: AlgebraicStructure mg u n
    => Symbol 
    -> AlgStruct mg u n 
    -> Either (PolySolveError mg u n) (AlgStruct mg u n)
factorize sym src =
    let 
        cfs = commonFactors src
        cfs' = Set.filter (sym ~?) cfs
    in if Set.size cfs == 0 then
        Left $ NoCommonFactors
    else if Set.size cfs' == 0 then
        Left $ TargetNotAFactor
    else do
        let commonFactorsOfInterest = Set.toList cfs'
        factored <- foldM factorOut src $ commonFactorsOfInterest
        pure . Multiplicative . NE.fromList $ factored:commonFactorsOfInterest

-- | Given a @target@ structure to try and factor out of another @src@
--   structure, this function returns the modified @src@ as if the given 
--   @target@ were not present. (i.e. factored out)
factorOut :: AlgebraicStructure mg u n 
    => AlgStruct mg u n 
    -> AlgStruct mg u n 
    -> Either (PolySolveError mg u n) (AlgStruct mg u n)
factorOut src target = 
    let 
        reportError = Left $ FactorNotFound target src
        factorOutM = mapM (flip factorOut target)
    in case src of 
        Additive ts
            -> Additive <$> factorOutM ts
        Multiplicative fs   
            -> let 
                newProd = case NE.filter (/= target) fs of
                    [single] -> single
                    multiple -> Multiplicative $ NE.fromList multiple
            in pure newProd
        _ -> reportError

commonFactors :: (AlgebraicStructure mg u n, Ord mg, Ord u, Ord n)
    => AlgStruct mg u n 
    -> Set.Set (AlgStruct mg u n)
commonFactors src = 
    case src of
        (Additive ts)    -> intersections . NE.map commonFactors $ ts
        other            -> factors other

factors :: (AlgebraicStructure mg u n, Ord mg, Ord u, Ord n)
    => AlgStruct mg u n 
    -> Set.Set (AlgStruct mg u n)
factors src = 
    case src of
        (Multiplicative fs) -> Set.unions $ NE.map factors fs
        (Negated n)         -> Set.map negate $ factors n
        (Inverse n)         -> Set.map recip $ factors n
        other               -> Set.singleton other

intersections :: Ord a => NE.NonEmpty (Set.Set a) -> Set.Set a
intersections [s] = s
intersections sets = foldl Set.intersection (NE.head sets) (NE.tail sets)
