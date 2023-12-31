module ConfigParser where

import Database.Persist.Postgresql (ConnectionPool)
import Options.Applicative
  ( Parser,
    auto,
    help,
    long,
    metavar,
    option,
    optional,
    short,
    showDefault,
    strOption,
    switch,
    value,
  )

data EnvConfig = EnvConfig
  { localpath :: FilePath,
    wavpath :: FilePath,
    recordingLength :: Int,
    audioRate :: Double,
    activationThreshold :: Double,
    sleepSeconds :: Double,
    segmentDuration :: Double,
    debug :: Bool,
    mailUser :: Maybe String,
    mailPassword :: Maybe String,
    mailServer :: String,
    ollamaHost :: String,
    ollamaPort :: Int,
    whisperHost :: String,
    whisperPort :: Int,
    sileroHost :: String,
    sileroPort :: Int,
    dbHost :: Maybe String,
    dbUser :: Maybe String,
    dbPassword :: Maybe String,
    dbName :: Maybe String
  }
  deriving (Show)

parseConfig :: Parser EnvConfig
parseConfig =
  EnvConfig
    <$> strOption
      ( long "scriptpath"
          <> metavar "FILEPATH"
          <> value ""
          <> help "the path for the helper scripts that record and emit messages"
      )
    <*> strOption
      ( long "wavpath"
          <> metavar "WAVPATH"
          <> value ""
          <> help "the path to write temporary audio splices"
      )
    <*> option
      auto
      ( long "recordingLength"
          <> help "how long each recording should last (in seconds)"
          <> showDefault
          <> value 600
          <> metavar "INT"
      )
    <*> option
      auto
      ( long "audioRate"
          <> help "audio sampling rate"
          <> showDefault
          <> value 16000.0
          <> metavar "FLOAT"
      )
    <*> option
      auto
      ( long "activationThreshold"
          <> short 'a'
          <> help "the portion of samples in a period required for demarcating an utterance"
          <> showDefault
          <> value 0.55
          <> metavar "FLOAT"
      )
    <*> option
      auto
      ( long "sleepSeconds"
          <> short 's'
          <> help "the length of time to wait for a recording after looking for a boundary"
          <> showDefault
          <> value 0.50
          <> metavar "FLOAT"
      )
    <*> option
      auto
      ( long "segmentDuration"
          <> help "duration of each period to look for voice"
          <> showDefault
          <> value 2.0
          <> metavar "FLOAT"
      )
    <*> switch
      ( long "debug"
          <> short 'd'
          <> help "Whether to print debug info"
      )
    <*> optional
      ( strOption $
          long "mailUser"
            <> help "the username to connect to gmail with"
      )
    <*> optional
      ( strOption $
          long "mailPassword"
            <> help "the password to connect to gmail with"
      )
    <*> strOption
      ( long "mailServer"
          <> value "smtp.gmail.com"
          <> showDefault
          <> help "smtp server for sending email"
      )
    <*> strOption
      ( long "ollamaHost"
          <> value "127.0.0.1"
          <> showDefault
          <> help "hostname or ip address of the ollama server"
      )
    <*> option
      auto
      ( long "ollamaPort"
          <> help "port for the ollama server completions"
          <> showDefault
          <> value 11434
          <> metavar "INT"
      )
    <*> strOption
      ( long "whisperHost"
          <> value "127.0.0.1"
          <> help "hostname or ip address of the ollama server"
      )
    <*> option
      auto
      ( long "whisperPort"
          <> help "port for the ollama server completions"
          <> showDefault
          <> value 5000
          <> metavar "INT"
      )
    <*> strOption
      ( long "sileroHost"
          <> value "127.0.0.1"
          <> help "hostname or ip address of the silero text-to-speech api"
      )
    <*> option
      auto
      ( long "whisperPort"
          <> help "port for the silero text-to-speech api"
          <> showDefault
          <> value 5002
          <> metavar "INT"
      )
    <*> optional
      ( strOption $
          long "dbHost"
            <> help "hostname or ip address of the database server"
      )
    <*> optional
      ( strOption $
          long "dbUser"
            <> help "database user name"
      )
    <*> optional
      ( strOption $
          long "dbPassword"
            <> help "database password"
      )
    <*> optional
      ( strOption $
          long "dbname"
            <> help "database name"
      )
