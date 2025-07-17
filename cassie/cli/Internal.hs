{-# LANGUAGE Safe #-}
module Internal 
    ( CassieCLI
    ) where

import Control.Monad.Reader (ReaderT)
import Settings (CassieJSON)

type CassieCLI = ReaderT CassieJSON IO 
