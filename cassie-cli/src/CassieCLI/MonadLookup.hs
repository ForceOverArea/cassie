{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module CassieCLI.MonadLookup 
    ( MonadLookup(..)
    ) where

import safe Control.Monad.Reader (ReaderT)
import safe Control.Monad.RWS (ask, get, RWST)
import safe Control.Monad.State (StateT)
import safe qualified Data.Map as Map
import safe Data.Maybe
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)

-- | A class for monad types that have a meaningful way of 
--   mapping some key type @k@ to some value type @a@. This 
--   class largely exists to allow monad transformer classes
--   that may already support this behavior with the aid of a
--   map-like type (e.g. @ReaderT r m@, @StateT s m@ where @r@, 
--   @s@ are some concrete @Map k a@) to be interchangeable with
--   file I/O actions within an @IO@ monad.
class Monad m => MonadLookup k a m | m -> k a where
    -- | Provides a mapping from a key type @k@ to a value type @a@
    --   courtesy of the monad context being accessed. (e.g. a 
    --   @(ReaderT (Map.Map k a) m)@ monad providing a literal map or
    --   @IO@ providing filesystem access.
    lookupM :: k -> m (Maybe a)

    -- | Provides a prefix to add to keys if it makes sense to have one.
    --   In the @IO@ monad, this is useful as it can indicate the current
    --   directory a process is running in. In other contexts, this 
    --   function is likely not as useful. By default, it returns @mempty@ 
    --   for the type @m k@.
    pathRoot :: Monoid (m k) => m k
    pathRoot = mempty

    -- | Similar to @MonadLookup.pathRoot@, but returns the current user's 
    --   home directory path within the IO monad. In other contexts, this 
    --   function is likely not as useful. By default, it returns @mempty@
    --   for the type @m k@.
    pathHome :: Monoid (m k) => m k
    pathHome = mempty

    -- | Indicates whether the given path exists in the monad context's map.
    memberM :: k -> m Bool
    memberM = fmap isJust . lookupM

instance (Monad m, Ord k) => MonadLookup k a (ReaderT (Map.Map k a) m) where
    lookupM k = Map.lookup k <$> ask

instance (Monad m, Monoid b, Ord k) => MonadLookup k a (RWST (Map.Map k a) b c m) where
    lookupM k = Map.lookup k <$> ask

instance (Monad m, Monoid c, Ord k) => MonadLookup k a (RWST b c (Map.Map k a) m) where
    lookupM k = Map.lookup k <$> get

instance (Monad m, Ord k) => MonadLookup k a (StateT (Map.Map k a) m) where
    lookupM k = Map.lookup k <$> get

instance MonadLookup FilePath String IO where
    lookupM fp = do
        pathIsReal <- doesFileExist fp
        if pathIsReal then
            Just <$> readFile fp
        else
            return Nothing

    pathRoot = getCurrentDirectory

    pathHome = getHomeDirectory