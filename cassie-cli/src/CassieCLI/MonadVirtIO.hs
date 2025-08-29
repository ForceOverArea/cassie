{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module CassieCLI.MonadVirtIO 
    ( MonadVirtIO(..)
    ) where

import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, getHomeDirectory, setCurrentDirectory)

class (Monad m) => MonadVirtIO m where 
    vReadFile :: FilePath -> m String

    vWriteFile :: FilePath -> String -> m ()

    vGetCurrentDirectory :: m FilePath

    vSetCurrentDirectory :: FilePath -> m ()

    vGetHomeDirectory :: m FilePath

    vDoesFileExist :: FilePath -> m Bool

    vCreateDirectoryIfMissing :: Bool -> FilePath -> m ()

    vTryReadFile :: FilePath -> m (Maybe String)
    vTryReadFile fp = do
        fileExists <- vDoesFileExist fp
        if fileExists then
            Just <$> vReadFile fp
        else 
            return Nothing

    vGetLine :: m String

    vPutStrLn :: String -> m ()

    vPrint :: (Show a) => a -> m ()
    vPrint = vPutStrLn . show

instance MonadVirtIO IO where
    vReadFile = readFile

    vWriteFile = writeFile

    vGetCurrentDirectory = getCurrentDirectory

    vSetCurrentDirectory = setCurrentDirectory

    vGetHomeDirectory = getHomeDirectory

    vDoesFileExist = doesFileExist

    vCreateDirectoryIfMissing = createDirectoryIfMissing

    vGetLine = getLine

    vPutStrLn = putStrLn