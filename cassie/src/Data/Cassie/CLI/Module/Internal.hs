module Data.Cassie.CLI.Module.Internal
    ( cassieBaseLibrary
    , cassieConfigDir
    , cassieFileExt
    , relPathDir
    , relPathFile
    , CassieModuleError(..)
    ) where

import Data.Cassie.CLI.Utils
import Data.List as List

data CassieModuleError
    = FileDoesNotExist FilePath
    | FoundRecursiveImport
    deriving (Eq, Show)

-- | The file extension used by Cassie source files.
cassieFileExt :: String
cassieFileExt = ".cas"

cassieBaseLibrary :: String
cassieBaseLibrary = "Base"

cassieConfigDir :: String
cassieConfigDir = "/.config/cassie"

relPathDir :: String -> String
relPathDir = relPathPart take

relPathFile :: String -> String
relPathFile = relPathPart drop

relPathPart :: (Int -> [String] -> [String]) -> String -> String
relPathPart f x = 
    let 
        segments = splitStrAt '/' x
        numSegs = length segments
    in if numSegs > 1 then
        intercalate "/" $ f (numSegs - 1) segments
    else 
        ""