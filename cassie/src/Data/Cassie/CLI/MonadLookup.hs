{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Cassie.CLI.MonadLookup 
    ( MonadLookup(..)
    ) where

import safe Control.Monad.Reader (ReaderT)
import safe Control.Monad.RWS (ask, get, RWST)
import safe Control.Monad.State (StateT)
import safe qualified Data.Map as Map
import System.Directory (doesFileExist)

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
