{-# LANGUAGE Safe #-}
module Data.Cassie.Parser.Lang 
    ( functionDef
    , isEqn
    , parseCassiePhrases
    , parseEquation
    , parseEquation'
    , parseFunction 
    , ParsedCtx
    , ParsedCtxItem
    , ParsedEqn
    , Phrase(..)
    , Symbols
    ) where

import safe Control.Arrow
import safe Data.Cassie.Parser.Internal
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Structures.Instances.Real (RealUnary, RealMagma)
import safe Data.Cassie.Structures.Internal (Symbol)
import safe Data.Cassie.Structures (Equation(..), RealEqn)
import safe Data.Cassie.Utils (splitStrAt, stripEndStr)
import safe qualified Data.Map as Map
import safe Data.List
import safe qualified Data.Set as Set
import safe Text.Parsec
import safe Text.Parsec.Token (GenTokenParser(..))
import safe Text.Parsec.Language (haskell)

type CassieLang a = Parsec String Symbols a

-- | The concrete type of @Context m u n@ that parsing Cassie syntax will yield.
type ParsedCtx = Context RealMagma RealUnary Double

-- | The concrete type of @CtxItem m u n@ that parsing Cassie syntax will yield.
type ParsedCtxItem = CtxItem RealMagma RealUnary Double

-- | This may change if/when support for matrices/complex numbers is added. 
type ParsedEqn = RealEqn

type Symbols = Set.Set Symbol

data Phrase
    = ParsedEqn (ParsedEqn, Symbols)
    | ParsedFn (Symbol, Symbols, ParsedCtxItem)
    deriving (Show, Eq, Ord)

isEqn :: Phrase -> Bool
isEqn (ParsedEqn _) = True
isEqn _ = False

parseCassiePhrases :: String -> String -> Either CassieParserError ([(ParsedEqn, Symbols)], ParsedCtx)
parseCassiePhrases sourcename source = 
    let 
        parseEqnsAndFuncs = partition isEqn 
            <$> runParser cassieFile Set.empty sourcename (stripEndStr source)
        
        addFuncToCtx ctx (ParsedFn (name, _, impl)) = Map.insert name impl ctx
        addFuncToCtx ctx _ = ctx

        foldCtx = foldl addFuncToCtx Map.empty 

        unwrapPhraseEqn (ParsedEqn eqn) = eqn
        unwrapPhraseEqn _ = error "branch of control flow not used"

        processPhrases = (map unwrapPhraseEqn) *** foldCtx
    in left FailedToParse $ processPhrases <$> parseEqnsAndFuncs


parseEquation :: String -> Either CassieParserError ParsedEqn
parseEquation source = left FailedToParse $ fst <$> runParser equation Set.empty source source

-- | This function is deprecated. Consider using @parseEquation@ instead. 
parseEquation' :: String -> Either CassieParserError (ParsedEqn, Set.Set Symbol)
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
cassieFile = do
    phrases <- phrase `sepEndBy1` char ';'
    eof
    return phrases

phrase :: CassieLang Phrase
phrase = 
    let 
        functionPhrase = ParsedFn <$> (whiteSpace haskell >> functionDef)
        equationPhrase = ParsedEqn <$> (whiteSpace haskell >> equation)
    in try functionPhrase <|> equationPhrase

equation :: CassieLang (ParsedEqn, Symbols)
equation = do
    leftHand <- expression 
    _ <- char '='
    syms <- getState    -- TODO: this feels like abuse of stateful behavior... 
    putState Set.empty
    eqn <- Equation leftHand <$> expression
    return (eqn, syms)

functionDef :: CassieLang (Symbol, Symbols, ParsedCtxItem)
functionDef = do
    _ <- string "fn"
    whiteSpace haskell
    name <- identifier haskell
    argNames <- parens haskell $ commaSep haskell $ identifier haskell
    _ <- string "->"
    whiteSpace haskell
    impl <- expression
    putState Set.empty -- TODO: this feels abusive too... see above comment
    return (name, Set.fromList argNames, Func argNames impl)