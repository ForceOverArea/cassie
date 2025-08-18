{-# LANGUAGE Trustworthy #-}
module JSFileSystem
    ( JSFileSystemT(runJSFileSystemT)
    ) where

import safe CassieCLI.MonadVirtFS (MonadVirtFS(..))
import safe Control.Monad.IO.Class (MonadIO(..))
import safe Control.Monad.Trans (MonadTrans(..))
import GHC.Wasm.Prim

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

instance MonadFail m => MonadFail (JSFileSystemT m) where
    fail s = lift $ fail s

instance MonadIO m => MonadIO (JSFileSystemT m) where
    liftIO = JSFileSystemT . liftIO

instance MonadFail m => MonadVirtFS (JSFileSystemT m) where
    vReadFile = return . fromJSString . fs_readFileSync . toJSString

    vWriteFile path contents = 
        let 
            pathJS     = toJSString path
            contentsJS = toJSString contents 
        in return $ fs_writeFileSync pathJS contentsJS

    vGetCurrentDirectory = return $ fromJSString process_cwd

    vSetCurrentDirectory path = 
        let 
            pathJS = toJSString path
        in return $ process_chdir pathJS

    vGetHomeDirectory = return $ fromJSString os_homedir

    vDoesFileExist = return . fs_lstat_isFile . toJSString

    vCreateDirectoryIfMissing p path = 
        let 
            pathJS = toJSString path
        in return $ fs_mkdir p pathJS

foreign import javascript fs_readFileSync :: JSString -> JSString
foreign import javascript fs_writeFileSync :: JSString -> JSString -> ()
foreign import javascript fs_lstat_isFile :: JSString -> Bool
foreign import javascript fs_mkdir :: Bool -> JSString -> ()
foreign import javascript os_homedir :: JSString
foreign import javascript process_cwd :: JSString
foreign import javascript process_chdir :: JSString -> ()