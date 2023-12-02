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

module ChatLoggerSchema where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, runStderrLoggingT)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.State as ST
import Data.Kind (Type)
import Data.Sequence.Internal.Sorting (Queue (Q))
import Data.Time (UTCTime)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Database.Persist (Entity (entityKey), PersistEntity (Key, PersistEntityBackend), PersistStoreRead (get), PersistStoreWrite (insert), SafeToInsert, selectList)
import Database.Persist.Postgresql
  ( BackendKey (SqlBackendKey),
    ConnectionPool,
    ConnectionString,
    Entity,
    PersistRecordBackend,
    SqlPersistT,
    runMigration,
    runSqlPersistMPool,
    selectList,
    withPostgresqlPool,
    (==.),
  )
import Database.Persist.SqlBackend (SqlBackend)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Listener (ListenerMonad, dbPool)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Query
    question String
    stamp UTCTime
    deriving Show
  Answer
    parent QueryId
    answer String
    deriving Show
|]
