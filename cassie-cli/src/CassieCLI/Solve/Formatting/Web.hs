{-# LANGUAGE Trustworthy #-}
module CassieCLI.Solve.Formatting.Web
    ( stringifyCtxAndSolution
    , JsonSolution(..)
    ) where

import safe Control.Arrow
import Data.Aeson 
import Data.Aeson.Key
import Data.Aeson.KeyMap
import Data.Aeson.Text
import safe Data.Cassie
import safe Data.Cassie.Rules
import safe Data.Cassie.Structures
import safe qualified Data.List as List
import safe qualified Data.Map as Map
import safe qualified Data.Text.Lazy as Text

newtype JsonCtx mg u n = JsonCtx { unJsonCtx :: Context mg u n }

newtype JsonCtxItem mg u n = JsonCtxItem { unJsonCtxItem :: CtxItem mg u n }

newtype JsonSolution mg u n = JsonSolution { unJson :: Solution mg u n }

newtype JsonSolnItem mg u n = JsonSolnItem { unJsonItem :: SolutionItem mg u n }

newtype JsonParsedElem n = JsonParsedElem { unJsonElem :: n }

instance (AlgebraicStructure mg u n) => ToJSON (JsonCtx mg u n) where
    toJSON = toJSON . Map.map JsonCtxItem . unJsonCtx

instance (AlgebraicStructure mg u n) => ToJSON (JsonCtxItem mg u n) where
    toJSON ci = case unJsonCtxItem ci of
        (Func args impl _) -> toJSON $ "(" ++ List.intercalate ", " args ++ ") -> " ++ showAlgStruct impl
        (Known val _) -> toJSON $ showAlgStruct val

instance (AlgebraicStructure mg u n) => ToJSON (JsonSolution mg u n) where
    toJSON = toJSON . Map.map JsonSolnItem . unJson

instance (AlgebraicStructure mg u n) => ToJSON (JsonSolnItem mg u n) where
    toJSON si = 
        let
            constrainedSoln = toJSON . show . constrained . unJsonItem $ si
            solnSteps       = toJSON . steps . unJsonItem $ si
            solnVal         = (toJSON ||| toJSON) . (show +++ JsonParsedElem) . possVal . unJsonItem $ si

            keymap = insert (fromString "possVal") solnVal
                   . insert (fromString "steps") solnSteps 
                   $ singleton (fromString "constrained") constrainedSoln
        in Object keymap

instance Show n => ToJSON (JsonParsedElem n) where
    toJSON = toJSON . show . unJsonElem

stringifyCtxAndSolution :: (AlgebraicStructure mg u n, Show a) => Either a (Context mg u n, Solution mg u n) -> String
stringifyCtxAndSolution = Text.unpack . encodeToLazyText . (show +++ (JsonCtx *** JsonSolution))