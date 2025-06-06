{-# LANGUAGE Safe #-}
module Data.Cassie.Parser.Lang 
    ( functionDef
    , parseCassieFile
    , parseEquation
    , parseFunction 
    , ParsedCtx
    , ParsedCtxItem
    , Phrase(..)
    , Symbols
    ) where

import safe Control.Arrow
import safe Data.Cassie.Parser.Internal
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Structures.Instances.Real (RealUnary, RealMagma)
import safe Data.Cassie.Structures.Internal (Symbol)
import safe Data.Cassie.Structures (Equation(..))
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set
import safe Text.Parsec
import safe Text.Parsec.Token (GenTokenParser(..))
import safe Text.Parsec.Language (haskell)

type CassieLang a = Parsec String Symbols a

-- | The concrete type of @Context m u n@ that parsing Cassie syntax will yield.
type ParsedCtx = Context RealMagma RealUnary Double

-- | The concrete type of @CtxItem m u n@ that parsing Cassie syntax will yield.
type ParsedCtxItem = CtxItem RealMagma RealUnary Double

type ParsedEqn = Equation RealMagma RealUnary Double

type Symbols = Set.Set Symbol

data Phrase
    = ParsedEqn ParsedEqn
    | ParsedFn (Symbol, Symbols, ParsedCtxItem)
    deriving (Show, Eq, Ord)

parseCassieFile :: String -> String -> Either CassieParserError [Phrase]
parseCassieFile sourcename source = left FailedToParse $ runParser cassieFile Set.empty sourcename source

parseEquation :: String -> Either CassieParserError ParsedEqn
parseEquation source = left FailedToParse $ runParser equation Set.empty source source

parseFunction :: ParsedCtx -> String -> Either CassieParserError ParsedCtx
parseFunction ctx funcDef = 
    let 
        parseResult = runParser parseConstrainedFunction Set.empty funcDef funcDef

        parseConstrainedFunction = do
            (name, argSyms, fnImpl) <- functionDef
            capSyms <- getState
            return (name, fnImpl, capSyms `Set.difference` argSyms)

        knowns = Set.fromList (Map.keys $ Map.filter isConst ctx)
    in do 
        (name, funcObj, dependencies) <- left FailedToParse parseResult
        if dependencies `Set.difference` knowns /= Set.empty then
            Left PoorlyDefinedError
        else
            return $ Map.insert name funcObj ctx

cassieFile :: CassieLang [Phrase]
cassieFile = phrase `sepBy1` char ';'

phrase :: CassieLang Phrase
phrase = 
    let 
        functionPhrase = ParsedFn <$> try functionDef
        equationPhrase = ParsedEqn <$> equation
    in try functionPhrase <|> equationPhrase

equation :: CassieLang ParsedEqn
equation = do
    leftHand <- expression 
    _ <- char '='
    Equation leftHand <$> expression

functionDef :: CassieLang (Symbol, Symbols, ParsedCtxItem)
functionDef = do
    _ <- string "fn"
    whiteSpace haskell
    name <- identifier haskell
    argNames <- parens haskell $ commaSep haskell $ identifier haskell
    _ <- string "->"
    whiteSpace haskell
    impl <- expression
    return (name, Set.fromList argNames, Func argNames impl)