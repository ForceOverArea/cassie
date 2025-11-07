{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedLists #-}
module CassieCLI.Parser.Lexemes 
    ( difference
    , expression
    , function
    , logarithm
    , p0Term
    , p1Term
    , p2Term
    , p3Term
    , parenthetical
    , parseExpression
    , product
    , quotient
    , sum
    , symb
    , CassieParser
    , Symbols
    ) where

import safe Prelude hiding (exponent, logBase, product, sum)
import safe Control.Monad
import safe qualified Data.List.NonEmpty as NE
import safe qualified Data.Set as Set
import safe Data.Cassie.Solver (Symbols) 
import safe Data.Cassie.Structures
import safe Text.Parsec
import safe Text.Parsec.Language (haskell)
import safe Text.Parsec.Token (GenTokenParser(..)) 

-- | Parser type for reading algebraic structures from plain text
type CassieParser = CassieParserInternal MixedAlgStruct

type CassieParserInternal = Parsec String Symbols

parseExpression :: String -> Either ParseError (MixedAlgStruct, Symbols)
parseExpression expr = 
    let 
        bundleFoundSyms struct = do
            syms <- getState
            pure (struct, syms)
    in runParser (expression >>= bundleFoundSyms) Set.empty expr expr

expression :: CassieParser 
expression = sum <?> "math expression"

sum :: CassieParser 
sum = do
    whiteSpace haskell
    elements <- NE.fromList 
        <$> p5Term `sepBy1` char '+' 
        <?> "sum"
    pure $ case elements of
        [x] -> x
        _   -> foldl (+) (NE.head elements) (NE.tail elements)

difference :: CassieParser 
difference = do
    minuend <- p4Term
    _ <- char '-'
    (minuend -) 
        <$> p5Term 
        <?> "difference"

product :: CassieParser 
product = do
    whiteSpace haskell
    elements <- NE.fromList 
        <$> p3Term `sepBy1` char '*' 
        <?> "product"
    pure $ case elements of
        [x] -> x
        _   -> foldl (*) (NE.head elements) (NE.tail elements)

quotient :: CassieParser 
quotient = do
    dividend <- p2Term
    _ <- char '/'
    (dividend /) 
        <$> p3Term 
        <?> "quotient"

exponent :: CassieParser 
exponent = do 
    expBase <- p1Term 
    _ <- char '^'
    Magma (MixedExpn) expBase 
        <$> p1Term 
        <?> "exponent"

logarithm :: CassieParser 
logarithm = whiteSpace haskell
    >> string "log"
    >> Magma (MixedLogm) 
        <$> angles haskell expression
        <*> parens haskell expression 
        <?> "logarithm"

root :: CassieParser 
root = whiteSpace haskell
    >> string "root"
    >> Magma (MixedRoot) 
        <$> angles haskell expression
        <*> parens haskell expression 
        <?> "root"

function :: CassieParser 
function = whiteSpace haskell
    >> N_ary 
        <$> identifier haskell 
        <*> parens haskell (commaSep haskell expression) 
        <?> "user-defined function"

parenthetical :: CassieParser 
parenthetical = whiteSpace haskell 
    >> parens haskell expression 
    <?> "grouped expression"

matrix :: CassieParser
matrix = 
    let
        matrixRow = commaSep1 haskell numberLiteral
    in do
        whiteSpace haskell
        rows <- (brackets haskell $ matrixRow `sepBy1` char ';') <?> "matrix"
        n <- case rows of 
            [] -> unexpected
                $ "expected at least one row when parsing matrix. "
                ++ "report this issue at: https://github.com/ForceOverArea/cassie/issues"
            [row] -> pure $ length row
            (row:others) -> let 
                    expectedNumCols = length row
                    sameNumberOfCols = flip $ (&&) . ((==) expectedNumCols . length)
                in if expectedNumCols == 0 then 
                    unexpected $ "expected at least one column when parsing matrix. "
                            ++ "report this issue at: https://github.com/ForceOverArea/cassie/issues"
                else if foldl' sameNumberOfCols True others then
                    pure expectedNumCols
                else
                    unexpected $ "expected all rows of matrix to have " 
                            ++ show expectedNumCols
                            ++ " elements"
        pure . Nullary . toMixedMatrix n $ join rows

-- | Parses an identifier (haskell definition) and returns an @AlgStruct.Symbol@
value :: CassieParser 
value = Nullary . Sclr <$> numberLiteral

numberLiteral :: CassieParserInternal Double
numberLiteral = try (float haskell) 
    <|> fromInteger <$> integer haskell 
    <?> "value"

-- | Parses a number literal and returns an @AlgStruct.Value@
symb :: CassieParser 
symb = do
    sym <- whiteSpace haskell
        >> identifier haskell 
        <?> "symbol"
    modifyState $ Set.insert sym 
    pure $ Symbol sym

-- Private functions for establishing priority of different lexemes

p5Term :: CassieParser 
p5Term = try difference <|> p4Term

p4Term :: CassieParser 
p4Term = try product <|> p3Term

p3Term :: CassieParser 
p3Term = try quotient <|> p2Term

p2Term :: CassieParser 
p2Term = try exponent <|> try root <|> p1Term

p1Term :: CassieParser 
p1Term = try logarithm <|> p0Term

p0Term :: CassieParser 
p0Term = try function <|> try parenthetical <|> try symb <|> try matrix <|> value
