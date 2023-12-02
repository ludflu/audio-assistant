{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ChatLogger where

import ChatLoggerSchema -- (Answer, Key (QueryKey), Query, migrateAll, parent)
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

runMigrations :: Maybe ConnectionPool -> IO ()
runMigrations = mapM_ (runSqlPersistMPool (runMigration migrateAll))

addAnswer :: Maybe ConnectionPool -> Answer -> IO ()
addAnswer dbPool answer = mapM_ (runSqlPersistMPool (insert answer)) dbPool

addQuery :: Maybe ConnectionPool -> Query -> IO (Maybe (Key Query))
-- addQuery :: (PersistEntityBackend record
--  ~ Database.Persist.Class.PersistStore.BaseBackend backend,
--  PersistStoreWrite backend,
--  Database.Persist.Class.PersistStore.BackendCompatible
--    SqlBackend backend,
--  MonadIO m, PersistEntity record, SafeToInsert record) =>
-- Maybe (resource-pool-0.4.0.0:Data.Pool.Internal.Pool backend)
-- -> record -> m (Maybe (Key record))
addQuery dbPool query = do
  case dbPool of
    Just pool -> liftIO $ flip runSqlPersistMPool pool $ do
      qid <- insert query
      return $ Just qid
    Nothing -> return Nothing

getAnswer :: (MonadIO m, MonadLogger m) => Entity Query -> SqlPersistT m [Entity Answer]
getAnswer q = selectList [AnswerParent ==. entityKey q] []
