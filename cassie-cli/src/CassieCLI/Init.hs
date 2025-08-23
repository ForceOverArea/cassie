{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
module CassieCLI.Init 
    ( cassieInitMain
    ) where

import safe CassieCLI.Internal
import safe CassieCLI.MonadVirtFS (MonadVirtFS(..))
import safe Control.Arrow
import safe Control.Monad.IO.Class (MonadIO(..))
import safe qualified Data.Text as Text

cassieInitMain :: (MonadVirtFS m, MonadIO m) => String -> [String] -> m () 
cassieInitMain projectName _argv = 
    let 
        replaceName schemaPath = Text.pack
            >>> Text.replace "<projectName>" (Text.pack projectName) 
            >>> Text.replace "<schemaPath>" (Text.pack schemaPath)
            >>> Text.unpack
    in do
        _ <- liftIO . putStrLn $ "running cassieInitMain!"
        jsonTemplatePath <- cassieJSONTemplate
        projectJSONFile <- vReadFile jsonTemplatePath
        schemaPath <- cassieJSONSchema
        vWriteFile "Cassie.json" $ replaceName schemaPath projectJSONFile
