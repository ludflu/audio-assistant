{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module ChatLogger where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, runStderrLoggingT)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.State as ST
import Data.Aeson.KeyMap (mapMaybe)
import Data.Kind (Type)
import Data.Sequence.Internal.Sorting (Queue (Q))
import Data.Time (UTCTime)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Database.Esqueleto.Experimental -- (ConnectionPool, Entity, PersistEntity (Key), SqlReadT, from, limit, orderBy, runMigration, runSqlPersistMPool, select, table, val, where_, (==.), (^.))
-- import Database.Persist (Entity (entityKey), PersistEntity (Key, PersistEntityBackend), PersistQueryRead (selectFirst), PersistStoreRead (get), PersistStoreWrite (insert), SafeToInsert, SelectOpt (Asc, LimitTo), selectList)
-- import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
-- import Database.Persist.SqlBackend (SqlBackend)
-- import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import GHC.Exts (Any)
import Listener (ListenerMonad, dbPool)

share
  [ mkPersist sqlSettings,
    mkMigrate "migrateAll"
  ]
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

runMigrations :: Maybe ConnectionPool -> IO ()
runMigrations = mapM_ (runSqlPersistMPool (runMigration migrateAll))

-- addAnswer :: Maybe ConnectionPool -> Answer -> IO ()
-- addAnswer dbPool answer = mapM_ (runSqlPersistMPool (insert answer)) dbPool

-- addQuery :: Maybe ConnectionPool -> Query -> IO (Maybe (Key Query))
-- addQuery dbPool query = do
--   case dbPool of
--     Just pool -> liftIO $ flip runSqlPersistMPool pool $ do
--       qid <- insert query
--       return $ Just qid
--     Nothing -> return Nothing

-- getAnswers :: (MonadIO m, MonadLogger m) => SqlReadT m [Entity Answer]

-- addAnswer :: Entity Answer -> (MonadIO m, MonadLogger m) => SqlPersistT m ()

addQuery :: (MonadIO m, MonadLogger m) => Entity Query -> SqlPersistT m ()
addQuery a = insertKey (entityKey a) (entityVal a)

addAnswer :: (MonadIO m, MonadLogger m) => Entity Answer -> SqlPersistT m ()
addAnswer a = insertKey (entityKey a) (entityVal a)

getAnswers :: (MonadIO m, MonadLogger m) => SqlReadT m [Entity Answer]
getAnswers = select $ from $ table @Answer -- where_ [a . AnswerParent ==. entityKey query]

getAnswersForQuery :: (MonadIO m, MonadLogger m) => SqlExpr (Entity Query) -> SqlReadT m [Entity Answer]
getAnswersForQuery query = select $ do
  a <- from $ table @Answer
  where_ (a ^. AnswerParent ==. query ^. QueryId)
  return a

-- getLastAnswers :: (MonadIO m, MonadLogger m) => Entity Query -> SqlPersistT m [Entity Answer]

-- getLastAnswers q = do
--   q <- select $ from $ table @Query orderBy [desc q . QueryStamp] limit 1
--   select $ from $ table @Answer where_ [a . AnswerParent ==. entityKey q]

-- getLastAnswer :: (MonadIO m, MonadLogger m) => Entity Query -> SqlPersistT m [Entity Answer]
-- getLastAnswer = do
--   qq <- getLastQuery
--   let q = head qq
--    in getAnswers q

-- getLastAnswer :: (MonadIO m, MonadLogger m) => Entity Query -> SqlPersistT m [Entity Answer]
-- getLastAnswer = do res <- rawSql "" []
--   forM_
--   results
--   $ \r -> do return r
