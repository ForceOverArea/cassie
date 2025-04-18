{-# LANGUAGE Safe #-}
module Data.Cassie.Parser 
    ( parseEquation
    , parseExpression
    ) where

import safe Prelude hiding (exponent, logBase, product, sum)

import safe Control.Arrow
import safe Data.Cassie.Structures(Equation(..), AlgebraicStruct(..))
import safe Data.Text (pack, split, strip, unpack)
import safe Text.Parsec
import safe Text.Parsec.Language (haskell)
import safe Text.Parsec.Token (GenTokenParser(..)) 

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

-- | Parser type for reading algebraic structures from plain text
type CassieParser = Parsec String () AlgebraicStruct

parseExpression :: String -> Either ParseError AlgebraicStruct
parseExpression expr = runParser expression () expr expr

parseEquation :: String -> Either CassieParserError Equation
parseEquation eqn =
    let 
        preprocess = map (unpack . strip) . split (== '=') . pack 
        sides = preprocess eqn
    in case sides of
        [lhs, rhs] -> do
            lhs' <- left FailedToParse $ parseExpression lhs 
            rhs' <- left FailedToParse $ parseExpression rhs
            return $ Equation (lhs', rhs')
        []         -> Left $ FoundNone eqn
        [_]        -> Left $ FoundExpression eqn
        (_:_:_)    -> Left $ FoundMultiple eqn

expression :: CassieParser
expression = sum

polyTerm :: Char -> ([AlgebraicStruct] -> AlgebraicStruct) -> CassieParser -> String -> CassieParser
polyTerm c cnstrctr p name = do
    elements <- p `sepBy1` char c <?> name
    return $ case elements of
        [x] -> x
        _   -> cnstrctr elements

sum :: CassieParser
sum = polyTerm '+' Sum difference "sum"

difference :: CassieParser
difference = polyTerm '-' Difference product "difference"

product :: CassieParser
product = polyTerm '*' Product p3Term "product"

quotient :: CassieParser
quotient = do
    dend <- p2Term
    _ <- char '/'
    sor <- p2Term <?> "quotient"
    return Quotient { dividend = dend
                    , divisor  = sor
                    }

exponent :: CassieParser
exponent = do
    expBase <- p1Term 
    _ <- char '^'
    expExp <- p1Term <?> "exponent"
    return Exponent { base = expBase
                    , expn = expExp
                    }

logarithm :: CassieParser
logarithm = do
    whiteSpace haskell
    _ <- string "log "
    logBase <- expression
    logLog <- parens haskell expression <?> "logarithm"
    return Logarithm { base = logBase
                     , logm = logLog
                     }

function :: CassieParser
function = do
    whiteSpace haskell
    funcName <- identifier haskell
    funcArgs <- parens haskell (commaSep haskell expression) <?> "user-defined function"
    return Function { fname = funcName 
                    , argv  = funcArgs
                    }

parenthetical :: CassieParser
parenthetical = Group <$> (whiteSpace haskell >> parens haskell expression) <?> "grouped expression"

-- | Parses an identifier (haskell definition) and returns an @AlgebraicStruct.Symbol@
value :: CassieParser
value = do
    val <- try (float haskell) <|> fromInteger <$> integer haskell <?> "value"
    return $ Value val

-- | Parses a number literal and returns an @AlgebraicStruct.Value@
symb :: CassieParser
symb = do
    sym <- whiteSpace haskell >> identifier haskell <?> "symbol"
    return $ Symbol sym

p3Term :: CassieParser
p3Term = try quotient <|> p2Term

p2Term :: CassieParser
p2Term = try exponent <|> p1Term

p1Term :: CassieParser
p1Term = try logarithm <|> p0Term

p0Term :: CassieParser
p0Term = try function <|> try parenthetical <|> try symb <|> value