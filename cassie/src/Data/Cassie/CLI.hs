{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Cassie.CLI 
    ( cassieConfigDir
    , cassieMain
    , ParsedCtx
    , ParsedCtxItem
    , ParsedElement
    , ParsedEqn
    , ParsedEqPool
    , ParsedMagma
    , ParsedSoln
    , ParsedUnary
    ) where

import safe Data.Cassie.CLI.Parser.ParsedTypes 
import safe Data.Cassie.CLI.Module
import safe Data.Cassie.CLI.MonadLookup
import safe Data.Cassie.Solver.Internal

cassieMain :: ( MonadLookup FilePath String m
              , Monoid (m FilePath)
              ) 
    => FilePath 
    -> Symbols
    -> m (Either ParsedCassieError (ParsedCtx, ParsedSoln), [String])
cassieMain = solveModular