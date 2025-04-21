{-# LANGUAGE Safe #-}
module Data.Cassie.Parser 
    ( parseEquation
    , parseExpression
    , CassieParserError
    ) where

import safe Prelude hiding (exponent, logBase, product, sum)

import safe Control.Arrow
import safe qualified Data.Set as Set
import safe Data.Cassie.Internal
import safe Data.Cassie.Parser.Internal
import safe Data.Cassie.Structures(AlgebraicStruct(..), Equation(..), Symbol)
import safe Text.Parsec

data CassieParserError 
    = FailedToParse ParseError
    | FoundExpression String
    | FoundMultiple String
    | FoundNone String

instance Show CassieParserError where
    show (FailedToParse err)
        = show "failed to parse equation: " ++ show err

    show (FoundExpression expr)
        = "given equation is an expression because it has no '=': " ++ expr

    show (FoundMultiple eqns)
        = "more than 1 '=' found in equation: " ++ eqns

    show (FoundNone eqn)
        = "found 0 expressions in given 'equation': " ++ eqn

parseExpression :: String -> Either ParseError (AlgebraicStruct, Set.Set Symbol)
parseExpression expr = 
    let 
        bundleFoundSyms struct = do
            syms <- getState
            return (struct, syms)
    in runParser (expression >>= bundleFoundSyms) Set.empty expr expr

parseEquation :: String -> Either CassieParserError (Equation, Set.Set Symbol)
parseEquation eqn =
    let 
        sides = splitStrAt '=' eqn
    in case sides of
        [lhs, rhs] -> do
            (lhs', lSyms) <- left FailedToParse $ parseExpression lhs 
            (rhs', rSyms) <- left FailedToParse $ parseExpression rhs
            return $ (Equation (lhs', rhs'), lSyms `Set.union` rSyms)
        []         -> Left $ FoundNone eqn
        [_]        -> Left $ FoundExpression eqn
        (_:_:_)    -> Left $ FoundMultiple eqn
