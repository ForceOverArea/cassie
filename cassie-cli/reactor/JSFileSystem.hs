{-# LANGUAGE Safe #-}
module JSFileSystem
    ( JSFileSystemT
    ) where

import safe 

newtype JSFileSystemT m a = JSFileSystemT { runJSFileSystemT :: m a }

instance Functor m => Functor (JSFileSystemT m) where
    fmap f (JSFileSystemT x) = JSFileSystemT $ fmap f x

instance Applicative m => Applicative (JSFileSystemT m) where
    pure x = JSFileSystemT $ pure x

    JSFileSystemT x <*> JSFileSystemT y = JSFileSystemT $ x <*> y

instance Monad m => Monad (JSFileSystemT m) where
    return = pure

    JSFileSystemT action >>= f = JSFileSystemT $ do
        result <- action
        runJSFileSystemT $ f result

instance MonadTrans JSFileSystemT where
    lift = JSFileSystemT

instance 