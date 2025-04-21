{-# LANGUAGE Safe #-}
module Data.Cassie.Parser.Internal
    ( difference
    , expression
    , function
    , logarithm
    , p0Term
    , p1Term
    , p2Term
    , p3Term
    , parenthetical
    , polyTerm
    , product
    , quotient
    , sum
    , symb
    , CassieParser
    ) where

import safe Prelude hiding (exponent, logBase, product, sum)
import safe qualified Data.Set as Set
import safe Data.Cassie.Structures (AlgebraicStruct(..), Symbol)
import safe Text.Parsec
import safe Text.Parsec.Language (haskell)
import safe Text.Parsec.Token (GenTokenParser(..)) 

-- | Parser type for reading algebraic structures from plain text
type CassieParser = Parsec String (Set.Set Symbol) AlgebraicStruct

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
    _ <- string "log"
    logBase <- angles haskell expression
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
    modifyState $ Set.insert sym
    return $ Symbol sym

p3Term :: CassieParser
p3Term = try quotient <|> p2Term

p2Term :: CassieParser
p2Term = try exponent <|> p1Term

p1Term :: CassieParser
p1Term = try logarithm <|> p0Term

p0Term :: CassieParser
p0Term = try function <|> try parenthetical <|> try symb <|> value
