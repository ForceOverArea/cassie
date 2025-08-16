{-# LANGUAGE Safe #-}
module CassieCLI.Solve.Internal 
    ( CassieCLI
    ) where

import Control.Monad.RWS (RWST)
import CassieCLI.Solve.Settings (CassieJSON)

type CassieCLI = RWST CassieJSON [String] () IO
