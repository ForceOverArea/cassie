{-# LANGUAGE Trustworthy #-}
module NodeIO
    ( NodeIOT(runNodeIOT)
    ) where

import safe CassieCLI.MonadVirtIO (MonadVirtIO(..))
import safe Control.Monad.IO.Class (MonadIO(..))
import safe Control.Monad.Trans (MonadTrans(..))
import GHC.Wasm.Prim

newtype NodeIOT m a = NodeIOT { runNodeIOT :: m a }

instance Functor m => Functor (NodeIOT m) where
    fmap f (NodeIOT x) = NodeIOT $ fmap f x

instance Applicative m => Applicative (NodeIOT m) where
    pure x = NodeIOT $ pure x

    NodeIOT x <*> NodeIOT y = NodeIOT $ x <*> y

instance Monad m => Monad (NodeIOT m) where
    return = pure

    NodeIOT action >>= f = NodeIOT $ do
        result <- action
        runNodeIOT $ f result

instance MonadTrans NodeIOT where
    lift = NodeIOT

instance MonadFail m => MonadFail (NodeIOT m) where
    fail s = lift $ fail s

instance MonadIO m => MonadIO (NodeIOT m) where
    liftIO = NodeIOT . liftIO

instance (MonadFail m, MonadIO m) => MonadVirtIO (NodeIOT m) where
    vReadFile path = liftIO $ fromJSString <$> (fs_readFileSync $ toJSString path)

    vWriteFile path contents = 
        let 
            pathJS     = toJSString path
            contentsJS = toJSString contents 
        in liftIO $ fs_writeFileSync pathJS contentsJS

    vGetCurrentDirectory = liftIO $ fromJSString <$> process_cwd

    vSetCurrentDirectory path = 
        let 
            pathJS = toJSString path
        in liftIO $ process_chdir pathJS

    vGetHomeDirectory = liftIO $ fromJSString <$> os_homedir

    vDoesFileExist = liftIO . fs_lstatSync_isFile . toJSString

    vCreateDirectoryIfMissing p path = 
        let 
            pathJS = toJSString path
        in liftIO $ fs_mkdirSync p pathJS

    vGetLine = liftIO $ fromJSString <$> readline_Interface_question

    vPutStrLn = liftIO . console_log . toJSString

foreign import javascript unsafe "console.log($1)"
    console_log :: JSString -> IO ()

foreign import javascript safe "globalThis.readline_Interface_question()"
    readline_Interface_question :: IO JSString

foreign import javascript unsafe "globalThis.fs_readFileSync($1)" 
    fs_readFileSync :: JSString -> IO JSString

foreign import javascript unsafe "globalThis.fs_writeFileSync($1, $2)" 
    fs_writeFileSync :: JSString -> JSString -> IO ()

foreign import javascript unsafe "globalThis.fs_lstatSync_isFile($1)" 
    fs_lstatSync_isFile :: JSString -> IO Bool

foreign import javascript unsafe "globalThis.fs_mkdirSync($1, $2)" 
    fs_mkdirSync :: Bool -> JSString -> IO ()

foreign import javascript unsafe "globalThis.os_homedir()" 
    os_homedir :: IO JSString

foreign import javascript unsafe "globalThis.process_cwd()" 
    process_cwd :: IO JSString

foreign import javascript unsafe "globalThis.process_chdir($1)" 
    process_chdir :: JSString -> IO ()
