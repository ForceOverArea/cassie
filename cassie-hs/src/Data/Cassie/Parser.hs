{-# LANGUAGE Safe #-}
module Data.Cassie.Parser 
    ( functionDef
    , parseCassieFile
    , parseEquation
    , parseEquation'
    , parseExpression
    , parseFunction 
    , CassieParserError
    , ParsedCtx
    , ParsedCtxItem
    , Phrase(..)
    , Symbols
    ) where

import safe Control.Arrow
import safe qualified Data.Set as Set
import safe Data.Cassie.Parser.Internal
import safe Data.Cassie.Parser.Lang
import safe Data.Cassie.Structures
import safe Text.Parsec
import safe Data.Cassie.Utils

parseExpression :: String -> Either ParseError (RealAlgStruct, Set.Set Symbol)
parseExpression expr = 
    let 
        bundleFoundSyms struct = do
            syms <- getState
            return (struct, syms)
    in runParser (expression >>= bundleFoundSyms) Set.empty expr expr

parseEquation' :: String -> Either CassieParserError (RealEqn, Set.Set Symbol)
parseEquation' eqn =
    let 
        sides = splitStrAt '=' eqn
    in case sides of
        [lText, rText] -> do
            (lhs', lSyms) <- left FailedToParse $ parseExpression lText
            (rhs', rSyms) <- left FailedToParse $ parseExpression rText
            return $ (Equation lhs' rhs', lSyms `Set.union` rSyms)
        []         -> Left $ FoundNone eqn
        [_]        -> Left $ FoundExpression eqn
        (_:_:_)    -> Left $ FoundMultiple eqn
