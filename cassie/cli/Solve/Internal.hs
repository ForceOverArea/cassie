{-# LANGUAGE Safe #-}
module Solve.Internal 
    ( CassieCLI
    ) where

import Control.Monad.RWS (RWST)
import Solve.Settings (CassieJSON)

type CassieCLI = RWST CassieJSON [String] () IO
