{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module Init 
    ( cassieInitMain
    ) where

import safe Control.Arrow
import safe Internal
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

