{-# LANGUAGE Safe #-}
module CassieCLI.Internal 
    ( cassieJSONSchema
    , cassieJSONTemplate
    , consoleLogger
    , ensureConfigDirectoryExists
    , noLogger
    , Logger
    ) where

import safe Control.Monad
import safe Control.Monad.IO.Class (MonadIO(..))
import safe CassieCLI.Module.Internal (cassieConfigDir)
import safe CassieCLI.MonadVirtFS(MonadVirtFS(..))

type Logger m = String -> m ()

cassieConfigPath :: MonadVirtFS m => m FilePath
cassieConfigPath =  (++ cassieConfigDir) <$> vGetHomeDirectory 

cassieBaseLibrary :: MonadVirtFS m => m FilePath
cassieBaseLibrary = (++ "/Base.cas") <$> cassieConfigPath

cassieJSONTemplate :: MonadVirtFS m => m FilePath
cassieJSONTemplate = (++ "/Template.json") <$> cassieConfigPath 

cassieJSONSchema :: MonadVirtFS m => m FilePath
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

noLogger :: MonadIO m => Logger m
noLogger _ = return ()

consoleLogger :: MonadIO m => Logger m
consoleLogger = liftIO . putStrLn

ensureConfigDirectoryExists :: (MonadVirtFS m, MonadIO m) => Logger m -> m ()
ensureConfigDirectoryExists logger = do
    jsonTemplatePath <- cassieJSONTemplate
    baseLibPath <- cassieBaseLibrary
    schemaPath <- cassieJSONSchema
    vCreateDirectoryIfMissing True =<< cassieConfigPath
    needJSONTemplate <- not <$> vDoesFileExist jsonTemplatePath
    when needJSONTemplate $ do
        vWriteFile jsonTemplatePath cassieJSONTemplateSource
        logger $ "INFO: cassie project template has been reset to default at " ++ jsonTemplatePath
    needBaseLibTemplate <- not <$> vDoesFileExist baseLibPath
    when needBaseLibTemplate $ do
        vWriteFile baseLibPath cassieBaseLibrarySource
        logger $ "INFO: cassie base library has been reset to default at " ++ baseLibPath
    needSchema <- not <$> vDoesFileExist schemaPath
    when needSchema $ do
        vWriteFile schemaPath cassieJSONSchemaSource
        logger $ "INFO: cassie project template schema has been reset to default at " ++ schemaPath
