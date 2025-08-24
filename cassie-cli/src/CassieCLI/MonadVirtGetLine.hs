{-# LANGUAGE Safe #-}
module CassieCLI.MonadVirtGetLine 
    ( MonadVirtGetLine(..)
    ) where

class (Monad m) => MonadVirtGetLine m where
    vGetLine :: m String

instance MonadVirtGetLine IO where
    vGetLine = getLine