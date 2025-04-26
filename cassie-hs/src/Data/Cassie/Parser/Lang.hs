{-# LANGUAGE Safe #-}
module Data.Cassie.Parser.Lang 
    ( functionDef
    , parseFunction 
    , CassieLangError
    , Symbols
    ) where

import safe Control.Arrow
import safe qualified Data.Map as Map
import safe qualified Data.Set as Set
import safe Data.Cassie.Evaluate (Context, CtxItem(..), isConst)
import safe Data.Cassie.Parser.Internal
import safe Data.Cassie.Structures (Symbol)
import safe Text.Parsec
import safe Text.Parsec.Token(GenTokenParser(..))
import safe Text.Parsec.Language (haskell)

type CassieLang a = Parsec String Symbols a

type Symbols = Set.Set Symbol

data CassieLangError 
    = CassieLangParseError ParseError
    | PoorlyDefinedError
    deriving Show

parseFunction :: Context -> String -> Either CassieLangError Context 
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

functionDef :: CassieLang (Symbol, Symbols, CtxItem)
functionDef = do
    _ <- string "fn"
    whiteSpace haskell
    name <- identifier haskell
    argNames <- parens haskell $ commaSep haskell $ identifier haskell
    _ <- string "->"
    whiteSpace haskell
    impl <- expression
    return (name, Set.fromList argNames, Func argNames impl)