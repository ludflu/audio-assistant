{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ChatLogger where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Time (UTCTime)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Database.Persist (PersistStoreWrite (insert))
import Database.Persist.Postgresql
  ( BackendKey (SqlBackendKey),
    ConnectionString,
    runMigration,
    runSqlPersistMPool,
    withPostgresqlPool,
  )
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Query
    question String
    stamp UTCTime
    deriving Show
  Answer
    queryId QueryId
    seq Int
    answer String
    deriving Show
|]

connStr :: ConnectionString
connStr = "host=localhost dbname=mydatabase user=myuser password=mypassword port=5432"

main :: IO ()
main =
  runStderrLoggingT $
    withPostgresqlPool connStr 10 $
      \pool ->
        liftIO $
          do
            now <- getCurrentTime
            flip runSqlPersistMPool pool $
              do
                runMigration migrateAll
                qid <- insert $ Query "why is the sky blue" now
                liftIO $ print qid
                return ()