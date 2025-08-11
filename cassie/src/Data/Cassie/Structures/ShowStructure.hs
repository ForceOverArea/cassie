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

newtype ShowAlgStruct mg u n = ShowAlgStruct (AlgStruct mg u n) deriving (Eq, Ord)

instance (ShowMagma mg, ShowUnary u, Show n, Num n) => Show (ShowAlgStruct mg u n) where
    show (ShowAlgStruct s) = 
        let
            renderTerms terms = 
                case NE.uncons terms of
                    (x, Just others) -> showAdd x ++ concatMap renderTerm others
                    (x, Nothing)     -> showAlgStruct x

            renderFactors factors = 
                let 
                    fraction = renderBoth $ partition inverted $ NE.toList factors
                in case fraction of
                    ([], num)    -> "(" ++ intercalate " * " num ++ ")"
                    (denom, [])  -> "1 / (" ++ intercalate " * " denom ++ ")"
                    (denom, num) -> "(" ++ intercalate " * " num ++ ") / (" ++ intercalate " * " denom ++ ")"

            renderTerm (Negated term) = showAdd (Negated term)
            renderTerm term           = " + " ++ showAdd term

            renderBoth = first (map unwrapInverse) >>> join (***) (map showMul)

            inverted (Inverse _) = True
            inverted _           = False

            unwrapInverse (Inverse x) = x
            unwrapInverse x = x
        in case s of
            Additive terms          -> renderTerms terms
            Multiplicative factors  -> renderFactors factors
            Negated neg             -> " - " ++ showNeg neg
            Inverse inv             -> "1 / " ++ showInv inv
            Magma m l r             -> showMagma m (ShowAlgStruct l) (ShowAlgStruct r)
            N_ary name args         -> name ++ "(" ++ intercalate "," (map showAlgStruct args) ++ ")"
            Unary u x               -> showUnary u (ShowAlgStruct x)
            Nullary num             -> show num
            Symbol sym              -> sym

showAlgStruct :: (ShowMagma mg, ShowUnary u, Show n, Num n) => AlgStruct mg u n -> String
showAlgStruct = show . ShowAlgStruct

showAlgStructWithin :: (ShowMagma mg, ShowUnary u, Show n, Num n) => AlgStruct mg u n -> AlgStruct mg u n -> String
showAlgStructWithin x y = 
    if x <%> y then
        "(" ++ showAlgStruct y ++ ")"
    else 
        showAlgStruct y

showAdd :: (ShowMagma mg, ShowUnary u, Show n, Num n) => AlgStruct mg u n -> String
showAdd = showAlgStructWithin (Additive . NE.singleton $ Nullary 1)

showMul :: (ShowMagma mg, ShowUnary u, Show n, Num n) => AlgStruct mg u n -> String
showMul = showAlgStructWithin (Multiplicative . NE.singleton $ Nullary 1)

showNeg :: (ShowMagma mg, ShowUnary u, Show n, Num n) => AlgStruct mg u n -> String
showNeg = showAlgStructWithin (Negated $ Nullary 1)

showInv :: (ShowMagma mg, ShowUnary u, Show n, Num n) => AlgStruct mg u n -> String
showInv = showAlgStructWithin (Inverse $ Nullary 1)

(<%>) :: (Show mg, Show u, Show n) => AlgStruct mg u n -> AlgStruct mg u n -> Bool
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

Negated _ <%> _ = False

Inverse _ <%> _ = False

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
