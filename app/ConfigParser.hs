module ConfigParser where

import Options.Applicative
import Data.Maybe

data EnvConfig = EnvConfig 
    { localpath ::   FilePath, recordingLength :: Int }

parseConfig :: Parser EnvConfig

parseConfig = EnvConfig 
    <$> strOption
        (long "scriptpath"
            <> metavar "SCRIPT"
            <> value ""
            <> help "the path for the helper scripts that record and emit messages")
    <*> option auto
          ( long "recordingLength"
         <> help "how long each recording should last (in seconds)"
         <> showDefault
         <> value 600
         <> metavar "INT" )

