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
import qualified Control.Monad.State as ST
import Data.Kind (Type)
import Data.Sequence.Internal.Sorting (Queue (Q))
import Data.Time (UTCTime)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Database.Persist (PersistEntity (Key, PersistEntityBackend), PersistStoreRead (get), PersistStoreWrite (insert), SafeToInsert)
import Database.Persist.Postgresql
  ( BackendKey (SqlBackendKey),
    ConnectionPool,
    ConnectionString,
    PersistRecordBackend,
    runMigration,
    runSqlPersistMPool,
    withPostgresqlPool,
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
    queryId QueryId
    answer String
    deriving Show
|]

addAnswer :: Maybe ConnectionPool -> Answer -> IO ()
addAnswer dbPool answer = do
  case dbPool of
    Just pool -> liftIO $ flip runSqlPersistMPool pool $ do
      insert answer
      return ()
    Nothing -> return ()

addQuery :: Maybe ConnectionPool -> Query -> IO (Maybe QueryId)
addQuery dbPool query = do
  case dbPool of
    Just pool -> liftIO $ flip runSqlPersistMPool pool $ do
      qid <- insert query
      return $ Just qid
    Nothing -> return Nothing
