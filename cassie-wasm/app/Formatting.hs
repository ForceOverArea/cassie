module Formatting
    ( 
    ) where

import Control.Monad.Reader (asks)
import Internal (CassieCLI)
import Settings (CassieJSON(..), CassieSolnOpts(..))

renderSymbolicSolns :: CassieCLI String
renderSymbolicSolns = do
    symbolicSolns <- asks $ symbolic . solution
    return ""