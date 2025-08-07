{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE LambdaCase #-}
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
import safe qualified Data.Text as Text

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
    { numeric     :: Maybe [String] 
    , constrained :: Maybe [String] 
    , symbolic    :: Maybe [String] 
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
        <$> v .:? "numeric"
        <*> v .:? "constrained"
        <*> v .:? "symbolic"

getSolnOptOrDefault :: (CassieSolnOpts -> Maybe [String]) -> Maybe CassieSolnOpts -> [String]
getSolnOptOrDefault g = \case 
    Just soln -> maybe [] id $ g soln
    Nothing -> ["*"]

constrained' :: Maybe CassieSolnOpts -> [String]
constrained' = getSolnOptOrDefault constrained

numeric' :: Maybe CassieSolnOpts -> [String]
numeric' = getSolnOptOrDefault numeric

symbolic' :: Maybe CassieSolnOpts -> [String]
symbolic' = getSolnOptOrDefault symbolic

parseCassieJSON :: String -> CassieJSON
parseCassieJSON = either error id . eitherDecodeStrictText . Text.pack
