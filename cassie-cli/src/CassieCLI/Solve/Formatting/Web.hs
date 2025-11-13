{-# LANGUAGE Trustworthy #-}
module CassieCLI.Solve.Formatting.Web
    ( stringifyPossSolution
    , stringifySolution
    , JsonSolution(..)
    ) where

import safe Control.Arrow
import Data.Aeson 
import Data.Aeson.Key
import Data.Aeson.KeyMap
import safe Data.Cassie
import safe qualified Data.Map as Map

newtype JsonSolution mg u n = JsonSolution { unJson :: Solution mg u n }

newtype JsonSolnItem mg u n = JsonSolnItem { unJsonItem :: SolutionItem mg u n }

newtype PossJsonSoln a mg u n = PossJsonSoln { unPossJson :: Either a (Solution mg u n) }

newtype JsonParsedElem n = JsonParsedElem { unJsonElem :: n }

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

instance (AlgebraicStructure mg u n, Show a) => ToJSON (PossJsonSoln a mg u n) where
    toJSON ps = 
        let 
            errors = either (toJSON . show) (const $ toJSON "") $ unPossJson ps
            solution = either (const $ toJSON "") (toJSON . JsonSolution) $ unPossJson ps

            keymap = insert (fromString "errors") errors
                   $ singleton (fromString "solution") solution
        in Object keymap

instance Show n => ToJSON (JsonParsedElem n) where
    toJSON = toJSON . show . unJsonElem

stringifySolution :: (AlgebraicStructure mg u n) => Solution mg u n -> String
stringifySolution = show . encode . JsonSolution

stringifyPossSolution :: (AlgebraicStructure mg u n, Show a) => Either a (Solution mg u n) -> String
stringifyPossSolution = show . encode . PossJsonSoln