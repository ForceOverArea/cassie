module Data.Cassie.CLI.Module.Internal
    ( CassieModuleError(..)
    ) where

data CassieModuleError
    = FileDoesNotExist FilePath
    | FoundRecursiveImport
    deriving (Eq, Show)