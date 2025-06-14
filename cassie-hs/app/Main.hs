module Main where

import Control.Arrow
import Control.Monad.Reader (asks, runReaderT, ReaderT)
import Data.Aeson (eitherDecodeStrictText)
import Data.Cassie (solveSystem)
-- import Data.Cassie.
import Data.Text (pack)
import Settings (parseCassieJSON, CassieJSON(..))

type CassieCLI = ReaderT CassieJSON IO 

main :: IO ()
main = do
    cassieJSON <- parseCassieJSON <$> readFile "./Cassie.json"
    entryPointSource <- readFile $ entryPoint cassieJSON
    return () 

cassieCliMain :: CassieCLI ()
cassieCliMain = do
    cassieSourceFilepath <- asks entryPoint
    let (imports, fnCtx, equations) = parseCassieSource cassieSourceFilepath
    return "ligma"
