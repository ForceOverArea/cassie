{-# LANGUAGE Safe #-}
module CassieCLI.Solve.Internal 
    ( CassieCLI
    ) where

import Control.Monad.RWS (RWST)
import CassieCLI.Solve.Settings (CassieJSON)

type CassieCLI m = RWST CassieJSON [String] () m
