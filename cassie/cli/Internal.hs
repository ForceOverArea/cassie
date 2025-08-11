{-# LANGUAGE Trustworthy #-}
module Internal 
    ( cassieJSONSchema
    , cassieJSONTemplate
    , consoleLogger
    , ensureConfigDirectoryExists
    , noLogger
    , Logger
    ) where

import Control.Monad
import Data.Cassie.CLI (cassieConfigDir)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)

type Logger = String -> IO ()

cassieConfigPath :: IO FilePath
cassieConfigPath =  (++ cassieConfigDir) <$> getHomeDirectory 

cassieBaseLibrary :: IO FilePath
cassieBaseLibrary = (++ "/Base.cas") <$> cassieConfigPath

cassieJSONTemplate :: IO FilePath
cassieJSONTemplate = (++ "/Template.json") <$> cassieConfigPath 

cassieJSONSchema :: IO FilePath
cassieJSONSchema = (++ "/CassieSchema.json") <$> cassieConfigPath

cassieJSONTemplateSource :: String
cassieJSONTemplateSource = "\
\{\n\
\    \"$schema\": \"<schemaPath>\",\n\
\    \"entryPoint\": \"<projectName>\",\n\
\    \"report\": {\n\
\        \"title\": \"<projectName>\",\n\
\        \"author\": \"\"\n\
\    },\n\
\    \"solution\": {\n\
\        \"whitelist\": false,\n\
\        \"numeric\":      [],\n\
\        \"constrained\":  [ \"x\" ],\n\
\        \"symbolic\":     []\n\
\    }\n\
\}"

cassieBaseLibrarySource ::String
cassieBaseLibrarySource = "\
\const pi = 3.14159; // The constant pi, representing the ratio of \n\
\                    // a circle's circumference to its diameter.\n\
\\n\
\const e = 2.71828;  // Euler's number, which when left-applied to \n\
\                    // exponentiation creates a function whose \n\
\                    // derivative is itself."

cassieJSONSchemaSource :: String
cassieJSONSchemaSource = "{\n\
\    \"$schema\": \"http://json-schema.org/draft-07/schema#\",\n\
\    \"description\": \"The Cassie project config file format.\",\n\
\    \"type\": \"object\",\n\
\    \"properties\": {\n\
\        \"entryPoint\": {\n\
\            \"type\": \"string\"\n\
\        },\n\
\        \"report\": {\n\
\            \"type\": \"object\",\n\
\            \"properties\": {\n\
\                \"title\": {\n\
\                    \"type\": \"string\"\n\
\                },\n\
\                \"author\": {\n\
\                    \"type\": \"string\"\n\
\                }\n\
\            }\n\
\        },\n\
\        \"solution\": {\n\
\            \"type\": \"object\",\n\
\            \"optional\": true,\n\
\            \"properties\": {\n\
\                \"constrained\": {\n\
\                    \"type\": \"array\",\n\
\                    \"optional\": \"true\",\n\
\                    \"items\": {\n\
\                        \"type\": \"string\"\n\
\                    }\n\
\                },\n\
\                \"numeric\": {\n\
\                    \"type\": \"array\",\n\
\                    \"optional\": \"true\",\n\
\                    \"items\": {\n\
\                        \"type\": \"string\"\n\
\                    }\n\
\                },\n\
\                \"symbolic\": {\n\
\                    \"type\": \"array\",\n\
\                    \"optional\": \"true\",\n\
\                    \"items\": {\n\
\                        \"type\": \"string\"\n\
\                    }\n\
\                }\n\
\            }\n\
\        }\n\
\    }\n\
\}"

noLogger :: Logger
noLogger _ = return ()

consoleLogger :: Logger
consoleLogger = putStrLn

ensureConfigDirectoryExists :: Logger -> IO ()
ensureConfigDirectoryExists logger = do
    jsonTemplatePath <- cassieJSONTemplate
    baseLibPath <- cassieBaseLibrary
    schemaPath <- cassieJSONSchema
    createDirectoryIfMissing True =<< cassieConfigPath
    needJSONTemplate <- not <$> doesFileExist jsonTemplatePath
    when needJSONTemplate $ do
        writeFile jsonTemplatePath cassieJSONTemplateSource
        logger $ "INFO: cassie project template has been reset to default at " ++ jsonTemplatePath
    needBaseLibTemplate <- not <$> doesFileExist baseLibPath
    when needBaseLibTemplate $ do
        writeFile baseLibPath cassieBaseLibrarySource
        logger $ "INFO: cassie base library has been reset to default at " ++ baseLibPath
    needSchema <- not <$> doesFileExist schemaPath
    when needSchema $ do
        writeFile schemaPath cassieJSONSchemaSource
        logger $ "INFO: cassie project template schema has been reset to default at " ++ schemaPath
