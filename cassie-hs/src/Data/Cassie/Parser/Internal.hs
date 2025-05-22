{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists #-}
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
import safe qualified Data.List.NonEmpty as NE
import safe qualified Data.Set as Set
import safe Data.Cassie.Structures.Internal (AlgStruct(..), Symbol)
import safe Data.Cassie.Structures.Magmas 
import safe Data.Cassie.Structures.Instances.Real
import safe Text.Parsec
import safe Text.Parsec.Language (haskell)
import safe Text.Parsec.Token (GenTokenParser(..)) 

-- | Parser type for reading algebraic structures from plain text
type CassieParser = Parsec String (Set.Set Symbol) (RealAlgStruct)

expression :: CassieParser
expression = sum

sum :: CassieParser
sum = polyTerm '+' Additive p5Term "sum"

difference :: CassieParser
difference = do
    minuend <- p4Term
    _ <- char '-'
    subtrahend <- p4Term <?> "difference"
    return $ Additive [minuend, Negated subtrahend]

product :: CassieParser
product = polyTerm '*' Multiplicative p3Term "product"

quotient :: CassieParser
quotient = do
    dividend <- p2Term
    _ <- char '/'
    divisor <- p2Term <?> "quotient"
    return $ Multiplicative [dividend, Inverse divisor]

exponent :: CassieParser
exponent = do
    expBase <- p1Term 
    _ <- char '^'
    expExp <- p1Term <?> "exponent"
    return $ Magma (RealMagma Expn) expBase expExp

logarithm :: CassieParser
logarithm = do
    whiteSpace haskell
    _ <- string "log"
    logBase <- angles haskell expression
    logLog <- parens haskell expression <?> "logarithm"
    return $ Magma (RealMagma Logm) logBase logLog

function :: CassieParser
function = do
    whiteSpace haskell
    funcName <- identifier haskell
    funcArgs <- parens haskell (commaSep haskell expression) <?> "user-defined function"
    return $ N_ary funcName funcArgs

parenthetical :: CassieParser
parenthetical = whiteSpace haskell >> parens haskell expression <?> "grouped expression"

-- | Parses an identifier (haskell definition) and returns an @AlgStruct.Symbol@
value :: CassieParser
value = do
    val <- try (float haskell) <|> fromInteger <$> integer haskell <?> "value"
    return $ Nullary val

-- | Parses a number literal and returns an @AlgStruct.Value@
symb :: CassieParser
symb = do
    sym <- whiteSpace haskell >> identifier haskell <?> "symbol"
    modifyState $ Set.insert sym
    return $ Symbol sym

p5Term :: CassieParser 
p5Term = try difference <|> p4Term

p4Term :: CassieParser 
p4Term = try product <|> p3Term

p3Term :: CassieParser
p3Term = try quotient <|> p2Term

p2Term :: CassieParser
p2Term = try exponent <|> p1Term

p1Term :: CassieParser
p1Term = try logarithm <|> p0Term

p0Term :: CassieParser
p0Term = try function <|> try parenthetical <|> try symb <|> value

polyTerm :: Char -> (NE.NonEmpty (RealAlgStruct) -> RealAlgStruct) -> CassieParser -> String -> CassieParser
polyTerm c cnstrctr p name = do
    whiteSpace haskell
    elements <- p `sepBy1` char c <?> name
    return $ case elements of
        [x] -> x
        _   -> cnstrctr $ NE.fromList elements
