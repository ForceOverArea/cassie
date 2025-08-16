{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module CassieCLI.Init 
    ( cassieInitMain
    ) where

import safe CassieCLI.Internal
import safe Control.Arrow
import safe qualified Data.Text as Text

cassieInitMain :: String -> [String] -> IO () 
cassieInitMain projectName _argv = 
    let 
        replaceName schemaPath = Text.pack
            >>> Text.replace "<projectName>" (Text.pack projectName) 
            >>> Text.replace "<schemaPath>" (Text.pack schemaPath)
            >>> Text.unpack
    in do
        jsonTemplatePath <- cassieJSONTemplate
        projectJSONFile <- readFile jsonTemplatePath
        schemaPath <- cassieJSONSchema
        writeFile "Cassie.json" $ replaceName schemaPath projectJSONFile

