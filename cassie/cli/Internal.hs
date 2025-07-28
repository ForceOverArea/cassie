{-# LANGUAGE Trustworthy #-}
module Internal 
    ( cassieBaseLibrary
    , cassieJSONTemplate
    , consoleLogger
    , ensureConfigDirectoryExists
    , noLogger
    , Logger
    ) where

import Control.Monad
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)

type Logger = String -> IO ()

cassieConfigPath :: IO FilePath
cassieConfigPath =  (++ "/.config/cassie") <$> getHomeDirectory 

cassieJSONTemplate :: IO FilePath
cassieJSONTemplate = (++ "/Template.json") <$> cassieConfigPath 

cassieBaseLibrary :: IO FilePath
cassieBaseLibrary = (++ "/Cassie.cas") <$> cassieConfigPath

cassieJSONTemplateSource :: String
cassieJSONTemplateSource = "\
\{\n\
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

noLogger :: Logger
noLogger _ = return ()

consoleLogger :: Logger
consoleLogger = putStrLn

ensureConfigDirectoryExists :: Logger -> IO ()
ensureConfigDirectoryExists logger = do
    jsonTemplatePath <- cassieJSONTemplate
    baseLibPath <- cassieBaseLibrary
    createDirectoryIfMissing True =<< cassieConfigPath
    needJSONTemplate <- not <$> doesFileExist jsonTemplatePath
    when needJSONTemplate $ do
        writeFile jsonTemplatePath cassieJSONTemplateSource
        logger $ "INFO: cassie project template has been reset to default at " ++ jsonTemplatePath
    needBaseLibTemplate <- not <$> doesFileExist baseLibPath
    when needBaseLibTemplate $ do
        writeFile baseLibPath cassieBaseLibrarySource
        logger $ "INFO: cassie base library has been reset to default at " ++ baseLibPath
