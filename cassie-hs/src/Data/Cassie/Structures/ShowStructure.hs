{-# LANGUAGE Safe #-}
module Data.Cassie.Structures.ShowStructure
    ( showAlgStruct
    ) where

import safe Control.Arrow 
import safe Control.Monad
import safe Data.Cassie.Structures.Internal
import safe Data.Cassie.Structures.Magmas
import safe Data.Cassie.Structures.UnarySystems
import safe Data.List as List
import safe qualified Data.List.NonEmpty as NE

newtype ShowAlgStruct m u n = ShowAlgStruct (AlgStruct m u n) deriving (Eq, Ord)

instance (ShowMagma m, ShowUnary u, Show n, Num n) => Show (ShowAlgStruct m u n) where
    show (ShowAlgStruct s) = 
        let
            renderTerm (Negated term) = showAdd (Negated term)
            renderTerm term           = " + " ++ showAdd term

            renderTerms terms = 
                case NE.uncons terms of
                    (x, Just others) -> showAdd x ++ concatMap renderTerm others
                    (x, Nothing)     -> showAlgStruct x

            inverted (Inverse _) = True
            inverted _           = False

            unwrapInverse (Inverse x) = x
            unwrapInverse x = x

            renderFactors factors = 
                let 
                    fraction = renderBoth $ partition inverted $ NE.toList factors
                    renderBoth = 
                        first (map unwrapInverse) 
                        >>> join (***) (map showMul)
                in case fraction of
                    ([], num)    -> "(" ++ intercalate "*" num ++ ")"
                    (denom, [])  -> "1 / (" ++ intercalate "*" denom ++ ")"
                    (denom, num) -> "(" ++ intercalate "*" num ++ ") / (" ++ intercalate "*" denom ++ ")"

        in case s of
            Additive terms          -> renderTerms terms
            Multiplicative factors  -> renderFactors factors
            Negated neg             -> " - " ++ showNeg neg
            Inverse inv             -> "1 / " ++ showInv inv
            Magma m l r             -> showMagma m l r
            N_ary name args         -> name ++ "(" ++ intercalate "," (map showAlgStruct args) ++ ")"
            Unary u x               -> showUnary u x
            Nullary num             -> show num
            Symbol sym              -> sym

showAlgStruct :: (ShowMagma m, ShowUnary u, Show n, Num n) => AlgStruct m u n -> String
showAlgStruct = show . ShowAlgStruct

showAlgStructWithin :: (ShowMagma m, ShowUnary u, Show n, Num n) => AlgStruct m u n -> AlgStruct m u n -> String
showAlgStructWithin x y = 
    if x <%> y then
        showAlgStruct x ++ "(" ++ showAlgStruct y ++ ")"
    else 
        showAlgStruct x ++ showAlgStruct y

showAdd :: (ShowMagma m, ShowUnary u, Show n, Num n) => AlgStruct m u n -> String
showAdd = showAlgStructWithin (Additive . NE.singleton $ Nullary 1)

showMul :: (ShowMagma m, ShowUnary u, Show n, Num n) => AlgStruct m u n -> String
showMul = showAlgStructWithin (Multiplicative . NE.singleton $ Nullary 1)

showNeg :: (ShowMagma m, ShowUnary u, Show n, Num n) => AlgStruct m u n -> String
showNeg = showAlgStructWithin (Negated $ Nullary 1)

showInv :: (ShowMagma m, ShowUnary u, Show n, Num n) => AlgStruct m u n -> String
showInv = showAlgStructWithin (Inverse $ Nullary 1)

(<%>) :: (Show m, Show u, Show n) => AlgStruct m u n -> AlgStruct m u n -> Bool
Additive _ <%> as =
    case as of
        -- Negated _   -> True
        -- Inverse _   -> True
        -- Magma _ _ _ -> True
        _           -> False

Multiplicative _ <%> as =
    case as of
        Additive _  -> True
        Negated _   -> True
        -- Magma _ _ _ -> True
        _           -> False

Negated _ <%> _ = True

Inverse _ <%> _ = True

Magma _ _ _ <%> as =
    case as of
        Additive _       -> True
        Multiplicative _ -> True
        Negated _        -> True
        Inverse _        -> True
        Magma _ _ _      -> True
        _                -> False

N_ary _ _ <%> _ = False

Unary _ _ <%> _ = False

Nullary _ <%> _ = False

Symbol _ <%> _ = False
