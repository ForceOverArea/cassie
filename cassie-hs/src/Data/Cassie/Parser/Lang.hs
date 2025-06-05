{-# LANGUAGE Safe #-}
module Data.Cassie.Parser.Lang 
    ( functionDef
    , parseFunction 
    , CassieLangError
    , ParsedCtx
    , ParsedCtxItem
    , Symbols
    ) where

import safe Control.Arrow
import safe Data.Cassie.Parser.Internal
import safe Data.Cassie.Rules.Evaluate
import safe Data.Cassie.Structures.Instances.Real (RealUnary, RealMagma)
import safe Data.Cassie.Structures.Internal (Symbol)
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

type Symbols = Set.Set Symbol

data CassieLangError 
    = CassieLangParseError ParseError
    | PoorlyDefinedError
    deriving Show

parseFunction :: ParsedCtx -> String -> Either CassieLangError ParsedCtx
parseFunction ctx funcDef = 
    let 
        parseResult = runParser parseConstrainedFunction Set.empty funcDef funcDef

        parseConstrainedFunction = do
            (name, argSyms, fnImpl) <- functionDef
            capSyms <- getState
            return (name, fnImpl, capSyms `Set.difference` argSyms)

        knowns = Set.fromList (Map.keys $ Map.filter isConst ctx)
    in do 
        (name, funcObj, dependencies) <- left CassieLangParseError parseResult
        if dependencies `Set.difference` knowns /= Set.empty then
            Left PoorlyDefinedError
        else
            return $ Map.insert name funcObj ctx

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