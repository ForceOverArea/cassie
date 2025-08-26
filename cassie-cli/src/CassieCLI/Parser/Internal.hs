{-# LANGUAGE Safe #-}
module CassieCLI.Parser.Internal
    ( CassieParserError(..)
    ) where

import Text.Parsec (ParseError)

data CassieParserError 
    = FailedToParse ParseError
    | FoundExpression String
    | FoundMultiple String
    | FoundNone String
    | PoorlyDefinedError
    deriving Eq

instance Show CassieParserError where
    show (FailedToParse err) = show "failed to parse equation: " ++ show err

    show (FoundExpression expr) = "given equation is an expression because it has no '=': " ++ expr

    show (FoundMultiple eqns) = "more than 1 '=' found in equation: " ++ eqns

    show (FoundNone eqn) = "found 0 expressions in given 'equation': " ++ eqn

    show PoorlyDefinedError = "the given function did not have all of its dependencies defined as arguments or in global context"
