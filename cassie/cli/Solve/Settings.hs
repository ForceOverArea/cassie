{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
module Solve.Settings 
    ( CassieJSON(..)
    , CassieReportOpts(..)
    , CassieSolnOpts(..)
    , constrained'
    , numeric'
    , symbolic'
    , parseCassieJSON
    ) where

import Data.Aeson (eitherDecodeStrictText)
import Data.Aeson.Types ((.:), (.:?), withObject, FromJSON(..))
import qualified Data.Text as Text

data CassieJSON = CassieJSON 
    { entryPoint :: FilePath
    , report     :: CassieReportOpts 
    , solution   :: Maybe CassieSolnOpts
    }
    deriving Show

data CassieReportOpts = CassieReportOpts 
    { title  :: String
    , author :: String
    }
    deriving Show

data CassieSolnOpts = CassieSolnOpts 
    { numeric     :: [String] 
    , constrained :: [String] 
    , symbolic    :: [String] 
    }
    deriving Show

instance FromJSON CassieJSON where
    parseJSON = withObject "Cassie.json" $ \v -> CassieJSON 
        <$> v .: "entryPoint"
        <*> v .: "report"
        <*> v .:? "solution"

instance FromJSON CassieReportOpts where
    parseJSON = withObject "report options" $ \v -> CassieReportOpts
        <$> v .: "title"
        <*> v .: "author"

instance FromJSON CassieSolnOpts where
    parseJSON = withObject "solution options" $ \v -> CassieSolnOpts
        <$> v .: "numeric"
        <*> v .: "constrained"
        <*> v .: "symbolic"

constrained' :: Maybe CassieSolnOpts -> [String]
constrained' = maybe ["*"] $ constrained

numeric' :: Maybe CassieSolnOpts -> [String]
numeric' = maybe ["*"] $ numeric

symbolic' :: Maybe CassieSolnOpts -> [String]
symbolic' = maybe ["*"] $ symbolic

parseCassieJSON :: String -> CassieJSON
parseCassieJSON = either error id . eitherDecodeStrictText . Text.pack
