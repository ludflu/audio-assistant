module ConfigParser where

import Options.Applicative
import Data.Maybe

data EnvConfig = EnvConfig 
    { 
      localpath :: FilePath, 
      wavpath :: FilePath, 
      recordingLength :: Int, 
      audioRate :: Double, 
      activationThreshold :: Double,
      sleepSeconds :: Double,
      segmentDuration :: Double, 
      debug :: Bool 
    }
    deriving Show

parseConfig :: Parser EnvConfig

parseConfig = EnvConfig 
    <$> strOption
        (long "scriptpath"
            <> metavar "FILEPATH"
            <> value ""
            <> help "the path for the helper scripts that record and emit messages")
    <*> strOption
        (long "wavpath"
            <> metavar "FILEPATH"
            <> value ""
            <> help "the path to write temporary audio splices")
    <*> option auto
          ( long "recordingLength"
         <> help "how long each recording should last (in seconds)"
         <> showDefault
         <> value 600
         <> metavar "INT" )
    <*> option auto
          ( long "audioRate"
         <> help "audio sampling rate"
         <> showDefault
         <> value 16000.0
         <> metavar "FLOAT" )
    <*> option auto
          ( long "activationThreshold"
         <> short 'a'
         <> help "the purportion of samples in a period required for demarcating an utterance"
         <> showDefault
         <> value 0.55
         <> metavar "FLOAT" )
    <*> option auto
          ( long "sleepSeconds"
         <> short 's'
         <> help "the length of time to wait for a recording after looking for a boundar7"
         <> showDefault
         <> value 0.5
         <> metavar "FLOAT" )
    <*> option auto
          ( long "segmentDuration"
         <> help "duration of each period to look for voice"
         <> showDefault
         <> value 2.0
         <> metavar "FLOAT" )
    <*> switch
          ( long "debug"
         <> short 'd'
         <> help "Whether to print debug info" )
    
     

