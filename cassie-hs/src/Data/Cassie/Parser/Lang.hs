{-# LANGUAGE Safe #-}
module Data.Cassie.Parser.Lang 
    ( functionDef
    , parseFunction 
    , CassieLangError
    ) where

import safe qualified Data.Map as Map
import safe qualified Data.Set as Set
import safe Control.Arrow (left)
import safe Data.Cassie.Evaluate (Context, CtxItem(..))
import safe Data.Cassie.Parser.Internal
import safe Data.Cassie.Structures (Symbol)
import safe Text.Parsec
import safe Text.Parsec.Token(GenTokenParser(..))
import safe Text.Parsec.Language (haskell)

type CassieLang a = Parsec String (Set.Set Symbol) a

data CassieLangError 
    = CassieLangParseError ParseError
    | PoorlyDefinedError
    deriving Show

parseFunction :: Context -> String -> Either CassieLangError Context 
parseFunction ctx funcDef = 
    let 
        parseResult = runParser parseConstrainedFunction Set.empty funcDef funcDef

        parseConstrainedFunction = do
            (name, struct) <- functionDef
            syms <- getState
            return (name, struct, syms)

        knowns = Set.fromList (Map.keys $ Map.filter isConst ctx)

        isConst (Const _)  = True
        isConst (Func _ _) = False
    in do 
        (name, funcObj, dependencies) <- left CassieLangParseError parseResult
        if dependencies `Set.difference` knowns /= Set.empty then
            Left PoorlyDefinedError
        else
            return $ Map.insert name funcObj ctx

functionDef :: CassieLang (Symbol, CtxItem)
functionDef = do
    _ <- string "fn"
    name <- identifier haskell
    argNames <- parens haskell $ commaSep haskell $ identifier haskell
    _ <- string "->"
    whiteSpace haskell
    impl <- expression
    return (name, Func argNames impl)