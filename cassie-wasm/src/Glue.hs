{-# LANGUAGE OverloadedStrings #-}
module Glue 
    ( cassieWrapper
    ) where

import Data.List
import qualified Data.Aeson as A
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Cassie (CassieError, Solution)
import Data.Cassie.Isolate (Steps)
import Data.Cassie.Structures (Equation, Symbol)

newtype SingleSoln = SingleSoln (String, Equation, Steps, Either CassieError Double)

instance A.ToJSON SingleSoln where
    toJSON (SingleSoln (sym, eqn, steps, val)) = 
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

cassieWrapper :: Either CassieError Solution -> String
cassieWrapper possSoln = show . A.encode
    $ case possSoln of
        Left err   -> jsonify err
        Right soln -> A.toJSON $ map (buildSingleSolnObject soln) (Map.keys soln)

buildSingleSolnObject :: Solution -> Symbol -> A.Value
buildSingleSolnObject soln name = 
    let 
        (symbolic, steps, numeric) = soln Map.! name
    in A.toJSON $ SingleSoln (name, symbolic, steps, numeric)

jsonify :: Show a => a -> A.Value
jsonify = (A.String . Text.show)
