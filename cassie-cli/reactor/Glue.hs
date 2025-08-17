{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Glue 
    ( cassieWrapper
    ) where

import safe Data.List
import safe qualified Data.Aeson as A
import safe qualified Data.Aeson.Key as AK
import safe qualified Data.Aeson.KeyMap as AKM
import safe qualified Data.Map as Map
import safe qualified Data.Text as Text
import safe Data.Cassie.Solver (CassieError, Solution, SolutionItem(..))
import safe Data.Cassie.Parser (ParsedEqn, ParsedCtxItem, ParsedCtx)
import safe Data.Cassie.Structures (Symbol)

newtype Solution' = Solution' (String, ParsedEqn, [String], Either CassieError Double)

instance A.ToJSON Solution' where
    toJSON (Solution' (sym, solvedEqn, work, val)) = 
        let 
            unwrap :: Either CassieError Double -> A.Value
            unwrap x = case x of
                Right y  -> jsonify y
                Left err -> jsonify err

            stepsLogStr = intercalate "\n" work
            
        in A.object [ "symbol"     A..= A.toJSON sym
                    , "equation"   A..= jsonify solvedEqn
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
        SolutionItem symbolic work numeric = soln Map.! name
    in A.toJSON $ Solution' (name, symbolic, work, numeric)

jsonify :: Show a => a -> A.Value
jsonify = (A.String . Text.show)
