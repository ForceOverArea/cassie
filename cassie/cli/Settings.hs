{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
module Settings 
    ( CassieJSON(..)
    , CassieReportOpts(..)
    , CassieSolnOpts(..)
    , parseCassieJSON
    ) where

import Data.Aeson (eitherDecodeStrictText)
import Data.Aeson.Types ((.:), withObject, FromJSON(..))
import qualified Data.Text as Text

data CassieJSON = CassieJSON 
    { entryPoint :: String
    , report     :: CassieReportOpts 
    , solution   :: CassieSolnOpts
    }
    deriving Show

data CassieReportOpts = CassieReportOpts 
    { title  :: String
    , author :: String
    }
    deriving Show

data CassieSolnOpts = CassieSolnOpts 
    { whitelist :: Bool
    , numeric   :: [String] 
    , constants :: [String] 
    , symbolic  :: [String] 
    }
    deriving Show

instance FromJSON CassieJSON where
    parseJSON = withObject "Cassie.json" $ \v -> CassieJSON 
        <$> v .: "entryPoint"
        <*> v .: "report"
        <*> v .: "solution"

instance FromJSON CassieReportOpts where
    parseJSON = withObject "report options" $ \v -> CassieReportOpts
        <$> v .: "title"
        <*> v .: "author"

instance FromJSON CassieSolnOpts where
    parseJSON = withObject "solution options" $ \v -> CassieSolnOpts
        <$> v .: "whitelist"
        <*> v .: "numeric"
        <*> v .: "constants"
        <*> v .: "symbolic"

parseCassieJSON :: String -> CassieJSON
parseCassieJSON = either error id . eitherDecodeStrictText . Text.pack