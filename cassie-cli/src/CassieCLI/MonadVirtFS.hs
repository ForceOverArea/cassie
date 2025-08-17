{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module CassieCLI.MonadVirtFS 
    ( MonadVirtFS(..)
    ) where

import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory, setCurrentDirectory)

class (Monad m, MonadFail m) => MonadVirtFS m where 
    vReadFile :: FilePath -> m String

    vWriteFile :: FilePath -> String -> m ()

    vGetCurrentDirectory :: m FilePath

    vSetCurrentDirectory :: FilePath -> m ()

    vGetHomeDirectory :: m FilePath

    vDoesFileExist :: FilePath -> m Bool

    vTryReadFile :: FilePath -> m (Maybe String)
    vTryReadFile fp = do
        fileExists <- vDoesFileExist fp
        if fileExists then
            Just <$> vReadFile fp
        else 
            return Nothing

instance MonadVirtFS IO where
    vReadFile = readFile

    vWriteFile = vWriteFile

    vGetCurrentDirectory = getCurrentDirectory

    vSetCurrentDirectory = setCurrentDirectory

    vGetHomeDirectory = getHomeDirectory

    vDoesFileExist = doesFileExist