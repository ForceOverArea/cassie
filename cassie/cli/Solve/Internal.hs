{-# LANGUAGE Safe #-}
module Solve.Internal 
    ( CassieCLI
    ) where

import Control.Monad.Reader (ReaderT)
import Solve.Settings (CassieJSON)

type CassieCLI = ReaderT CassieJSON IO 
