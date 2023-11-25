{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ChatLogger where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (ReaderT)
import Data.Kind (Type)
import Data.Time (UTCTime)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Database.Persist (PersistEntity (Key, PersistEntityBackend), PersistStoreWrite (insert), SafeToInsert)
import Database.Persist.Postgresql
  ( BackendKey (SqlBackendKey),
    ConnectionPool,
    ConnectionString,
    PersistRecordBackend,
    runMigration,
    runSqlPersistMPool,
    withPostgresqlPool,
  )
import Database.Persist.SqlBackend
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

type Bla = forall record (m :: Type -> Type). (MonadIO m, PersistRecordBackend record SqlBackend, SafeToInsert record) => record -> ReaderT SqlBackend m (Key record)

addAnswer :: Bla
addAnswer answer = insert answer
