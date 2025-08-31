{-# LANGUAGE Safe #-}
module CassieCLI.Parser.ParsedTypes 
    ( ParsedAlgStruct
    , ParsedCtx
    , ParsedCtxItem
    , ParsedElement
    , ParsedEqn
    , ParsedEqPool
    , ParsedMagma
    , ParsedSoln
    , ParsedUnary
    ) where

import safe Data.Cassie.Rules (Context, CtxItem)
import safe Data.Cassie.Solver
import safe Data.Cassie.Structures

-- | The concrete type of @Context mg u n@ that parsing Cassie syntax will yield.
type ParsedCtx = Context ParsedMagma ParsedUnary ParsedElement

-- | The concrete type of @CtxItem mg u n@ that parsing Cassie syntax will yield.
type ParsedCtxItem = CtxItem ParsedMagma ParsedUnary ParsedElement

-- | The concrete type of @Equation mg u n@ that parsing Cassie syntax will yield.
type ParsedEqn = Equation ParsedMagma ParsedUnary ParsedElement

-- | A concrete instance of @EquationPool mg u n@ used by higher-level Cassie code.  
type ParsedEqPool = EquationPool ParsedMagma ParsedUnary ParsedElement

-- | A concrete instance of @Solution mg u n@ used by higher-level Cassie code.
type ParsedSoln = Solution ParsedMagma ParsedUnary ParsedElement

-- | A concrete instance of @AlgStruct mg u n@ used by higher-level Cassie code.
type ParsedAlgStruct = AlgStruct ParsedMagma ParsedUnary ParsedElement

{-
    Below types can be changed to alter what data is parsed from Cassie syntax 
-}
type ParsedMagma = RealMagma
type ParsedUnary = RealUnary
type ParsedElement = Double