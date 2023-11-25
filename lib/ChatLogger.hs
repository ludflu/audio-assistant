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
  Person
    name String
    age Int
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
            flip runSqlPersistMPool pool $
              do
                runMigration migrateAll
                johnId <- insert $ Person "John Doe" 35
                liftIO $ print johnId
                return ()