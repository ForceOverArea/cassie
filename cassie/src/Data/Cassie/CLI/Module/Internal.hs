module Data.Cassie.CLI.Module.Internal
    ( cassieBaseLibrary
    , cassieConfigDir
    , cassieFileExt
    , CassieModuleError(..)
    ) where

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