{-# LANGUAGE Safe #-}
module Data.Cassie.Rules.Isolate.PolySolve 
    ( commonFactors
    , factorOut
    , factorize
    , factors
    , FactorizationError
    ) where

import safe Control.Monad
import safe qualified Data.Set as Set
import safe Data.Cassie.Structures.Internal ((~?), AlgStruct(..), Symbol)

data FactorizationError m u n
    = FactorNotFound (AlgStruct m u n) (AlgStruct m u n)
    | NoCommonFactors
    | TargetNotAFactor
    deriving Show

factorize :: Symbol -> AlgStruct m u n -> Either FactorizationError (AlgStruct m u n)
factorize sym src =
    let 
        cfs = commonFactors src
        cfs' = Set.filter (~? sym) cfs
    in if Set.size cfs == 0 then
        Left $ NoCommonFactors
    else if Set.size cfs' == 0 then
        Left $ TargetNotAFactor
    else do
        let commonFactorsOfInterest = Set.toList cfs'
        factored <- foldM (flip factorOut) src $ commonFactorsOfInterest
        return . Product $ commonFactorsOfInterest ++ [Group factored]

factorOut :: AlgStruct m u n -> AlgStruct m u n -> Either FactorizationError (AlgStruct m u n)
factorOut target src = 
    let 
        reportError = Left $ FactorNotFound target src
        factorOutM = mapM (factorOut target)
    in case src of 
        Additive ts          -> Additive <$> factorOutM ts
        Multiplicative fs
            | null fs   -> reportError
            | otherwise 
                -> let 
                    newProd = case filter (/= target) fs of
                        [single] -> single
                        multiple -> Product multiple
                in return newProd
        Quotient d s
            | d == target 
                        -> return $ Quotient (Value 1.0) s
            | s == reciprocal target 
                        -> return $ d
            | otherwise -> reportError
        _               -> reportError

commonFactors :: AlgStruct m u n -> Set.Set AlgStruct m u n
commonFactors src = 
    case src of
        (Sum ts)         -> intersections $ map commonFactors ts
        (Difference shs) -> intersections $ map commonFactors shs
        (Group g)        -> commonFactors g
        other            -> factors other

factors :: AlgStruct m u n -> Set.Set AlgStruct m u n
factors src = 
    case src of
        (Product fs)   -> Set.unions $ map factors fs
        (Quotient d s) -> factors d `Set.union` (Set.map reciprocal $ factors s) 
        (Group g)      -> factors g
        other          -> Set.singleton other

intersections :: Ord a => [Set.Set a] -> Set.Set a
intersections [] = Set.empty
intersections [s] = s
intersections (s:ss) = foldl Set.intersection s ss
