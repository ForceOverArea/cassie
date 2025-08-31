{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module JSHelper 
    ( evaluateStr
    , factorizeStr
    , isolateStr
    , substituteStr
    ) where

import safe Control.Arrow
import safe Data.Cassie.Rules (evaluate, factorize, isolate, substitute)
import safe Data.Cassie.Structures (showAlgStruct)
import safe CassieCLI.Parser 
    ( parseEquation
    , parseExpression 
    , ParsedEqn
    , ParsedAlgStruct
    )

evaluateStr :: String -> String
evaluateStr expr = unEither $ do
    struct <- parseExpressionStr expr 
    (show +++ show) $ evaluate mempty struct

factorizeStr :: String -> String -> String
factorizeStr target expr = unEither $ do
    struct <- parseExpressionStr expr
    (show +++ showAlgStruct) $ factorize target struct

isolateStr :: String -> String -> String
isolateStr target eqn = unEither $ do
    parsedEqn <- parseEquationStr eqn
    (show +++ show . fst) $ isolate mempty parsedEqn target 

substituteStr :: String -> String -> String -> String
substituteStr target replacement src = unEither $ do
    parsedReplacement <- parseExpressionStr replacement
    parsedSrc <- parseExpressionStr src
    (show +++ showAlgStruct) $ substitute target parsedReplacement parsedSrc

-- Utility functions 

parseExpressionStr :: String -> Either String ParsedAlgStruct
parseExpressionStr = (show +++ fst) . parseExpression 

parseEquationStr :: String -> Either String ParsedEqn
parseEquationStr = (show +++ fst) . parseEquation

unEither :: Either a a -> a
unEither = id ||| id