{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Cassie.Parser.Lang 
    ( functionDef
    , parseCassiePhrases
    , parseEquation
    , parseEquation'
    , parseFunction 
    , Import
    , ParsedCtx
    , ParsedCtxItem
    , ParsedEqn
    , Phrase(..)
    ) where

import safe Control.Arrow
import safe Data.Cassie.Parser.Internal
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Structures.Instances.Real (RealUnary, RealMagma)
import safe Data.Cassie.Structures.Internal (Symbol)
import safe Data.Cassie.Structures (Equation(..), RealEqn)
import safe Data.Cassie.Utils (splitStrAt)
import safe qualified Data.Map as Map
import safe Data.List as List
import safe qualified Data.Set as Set
import safe qualified Data.Text as Text
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

type Import = (String, [Symbol])

data Phrase
    = ParsedConst  (Symbol, Symbols, ParsedCtxItem)
    | ParsedEqn    (ParsedEqn, Symbols)
    | ParsedFn     (Symbol, Symbols, ParsedCtxItem)
    | ParsedImport (String, [Symbol])
    deriving (Show, Eq, Ord)

parseCassiePhrases :: String -> String -> Either CassieParserError ([(ParsedEqn, Symbols)], ParsedCtx, [Import])
parseCassiePhrases sourcename source = 
    let 
        processPhrases x = ( foldl addEqnToPool mempty x
                           , foldl addFuncToCtx (foldl addConstToCtx mempty x) x
                           , foldl addImport mempty x
                           )

        addConstToCtx ctx (ParsedConst (name, _, expr)) = Map.insert name expr ctx 
        addConstToCtx ctx _ = ctx

        addEqnToPool pool (ParsedEqn eqnAndSyms) = eqnAndSyms:pool
        addEqnToPool pool _ = pool

        addFuncToCtx ctx (ParsedFn (name, _, impl)) = Map.insert name impl ctx
        addFuncToCtx ctx _ = ctx

        addImport importList (ParsedImport importInfo) = importInfo:importList
        addImport importList _ = importList

    in left FailedToParse 
        $ processPhrases 
        <$> runParser 
            cassieFile 
            Set.empty 
            sourcename 
            (preprocessSource source)

parseEquation :: String -> Either CassieParserError (ParsedEqn, Symbols)
parseEquation source = left FailedToParse $ runParser equation Set.empty source source

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
cassieFile = manyTill phrase eof

phrase :: CassieLang Phrase
phrase = 
    let 
        constantPhrase = ParsedConst <$> (whiteSpace haskell >> constant)
        equationPhrase = ParsedEqn <$> (whiteSpace haskell >> equation)
        functionPhrase = ParsedFn <$> (whiteSpace haskell >> functionDef)
        importPhrase = ParsedImport <$> (whiteSpace haskell >> importStatement)
    in importPhrase 
        <|> try constantPhrase 
        <|> try functionPhrase 
        <|> equationPhrase

-- Top-level lexemes for top-level solver application

constant :: CassieLang (Symbol, Symbols, ParsedCtxItem)
constant = do
    name <- whiteSpace haskell 
        >> string "const"
        >> identifier haskell
    expr <- char '='
        >> expression
    syms <- getSymsAndReset
    _ <- semi haskell 
        <?> "constant"
    return (name, syms, Const expr)

equation :: CassieLang (ParsedEqn, Symbols)
equation = do
    leftHand <- expression 
    eqn <- char '='
        >> Equation leftHand <$> expression
    syms <- getSymsAndReset
    _ <- semi haskell 
        <?> "equation"
    return (eqn, syms)

functionDef :: CassieLang (Symbol, Symbols, ParsedCtxItem)
functionDef = do
    name <- string "fn" 
        >> whiteSpace haskell 
        >> identifier haskell
    argNames <- string "->"
        >> parens haskell (commaSep haskell $ identifier haskell)
    impl <- whiteSpace haskell 
        >> expression
    semi haskell
        >> putState Set.empty 
        <?> "function definition"
    return (name, Set.fromList argNames, Func argNames impl)

importStatement :: CassieLang (String, [Symbol])
importStatement = do
    pathSegments <- reserved haskell "import" 
        >> sepBy1 (dot haskell) (identifier haskell)
    imports <- parens haskell (commaSep1 haskell $ identifier haskell) 
        <?> "import statement"
    return (intercalate "/" pathSegments, imports)

-- Language-specific utility functions

-- | Removes comments from the source and strips trailing whitespace to simplify parser logic. 
preprocessSource :: String -> String
preprocessSource = 
    let 
        scrubComments original = 
            case List.uncons $ Text.splitOn "//" original of
                Just (source, _) -> source
                Nothing -> original

        joinBack linesOfText = 
            case List.uncons linesOfText of
                Just (h, t) -> foldl ((<>) . (<> "\n")) h t
                Nothing -> error "branch of control flow not used."
    in Text.unpack . Text.stripEnd . joinBack . map scrubComments . Text.split (== '\n') . Text.pack

getSymsAndReset :: CassieLang Symbols
getSymsAndReset = do
    parsedSyms <- getState
    putState Set.empty
    return parsedSyms