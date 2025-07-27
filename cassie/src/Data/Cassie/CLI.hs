{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Cassie.CLI 
    ( cassieMain
    ) where

import safe Control.Monad.Except (MonadError)
import safe Data.Cassie.CLI.Parser.ParsedTypes 
import safe Data.Cassie.CLI.Module
import safe Data.Cassie.CLI.MonadLookup

cassieMain :: ( MonadError ParsedCassieError m
              , MonadLookup FilePath String m
              , Monoid (m FilePath)
              ) 
    => FilePath 
    -> m (Either ParsedCassieError (ParsedCtx, ParsedSoln))
cassieMain = solveModular