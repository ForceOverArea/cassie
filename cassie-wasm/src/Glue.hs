{-# LANGUAGE OverloadedStrings #-}
module Glue 
    ( cassieWrapper
    ) where

import Data.List
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Cassie (CassieError, Solution)
import Data.Cassie.Parser
import Data.Cassie.Structures (Symbol)

newtype Solution' = Solution' (String, ParsedEqn, [String], Either CassieError Double)

instance A.ToJSON Solution' where
    toJSON (Solution' (sym, eqn, steps, val)) = 
        let 
            unwrap :: Either CassieError Double -> A.Value
            unwrap x = case x of
                Right y  -> jsonify y
                Left err -> jsonify err

            stepsLogStr = intercalate "\n" steps

        in A.object [ "symbol"     A..= A.toJSON sym
                    , "equation"   A..= jsonify eqn
                    , "steps"      A..= A.toJSON stepsLogStr
                    , "maybeValue" A..= unwrap val
                    ]

newtype CtxItem' = CtxItem' (ParsedCtxItem)

instance A.ToJSON CtxItem' where
    toJSON (CtxItem' ci) = A.toJSON $ show ci

cassieWrapper :: Either CassieError (ParsedCtx, Solution) -> String
cassieWrapper possSoln = show . A.encode
    $ case possSoln of
        Left err -> jsonify err
        Right (ctx, soln) -> A.object [ "context"  A..= buildJSONContext ctx
                                      , "solution" A..= buildJSONSoln soln
                                      ]

buildJSONContext :: ParsedCtx -> A.Value
buildJSONContext = A.toJSON . AKM.fromMap . Map.map CtxItem' . Map.mapKeys AK.fromString

buildJSONSoln :: Solution -> A.Value
buildJSONSoln soln = A.toJSON $ map (buildSingleSolnObject soln) (Map.keys soln)

buildSingleSolnObject :: Solution -> Symbol -> A.Value
buildSingleSolnObject soln name = 
    let 
        (symbolic, steps, numeric) = soln Map.! name
    in A.toJSON $ Solution' (name, symbolic, steps, numeric)

jsonify :: Show a => a -> A.Value
jsonify = (A.String . Text.show)
