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
import Database.Esqueleto.Experimental
  ( BackendKey (SqlBackendKey),
    ConnectionPool,
    Entity (entityKey, entityVal),
    PersistStoreWrite (insertKey),
    SqlExpr,
    SqlPersistT,
    SqlReadT,
    from,
    runMigration,
    runSqlPersistMPool,
    select,
    table,
    where_,
    (==.),
    (^.),
  )
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

addAnswer :: Maybe ConnectionPool -> Entity Answer -> IO ()
addAnswer dbPool answer = mapM_ (runSqlPersistMPool (addAnswer' answer)) dbPool

addQuery :: Maybe ConnectionPool -> Entity Query -> IO ()
addQuery dbPool query = mapM_ (runSqlPersistMPool (addQuery' query)) dbPool

-- addQuery :: Maybe ConnectionPool -> Query -> IO (Maybe (Key Query))
-- addQuery dbPool query = do
--   case dbPool of
--     Just pool -> liftIO $ flip runSqlPersistMPool pool $ do
--       qid <- insert query
--       return $ Just qid
--     Nothing -> return Nothing

-- getAnswers :: (MonadIO m, MonadLogger m) => SqlReadT m [Entity Answer]

-- addAnswer :: Entity Answer -> (MonadIO m, MonadLogger m) => SqlPersistT m ()

addQuery' :: (MonadIO m, MonadLogger m) => Entity Query -> SqlPersistT m ()
addQuery' q = insertKey (entityKey q) (entityVal q)

addAnswer' :: (MonadIO m, MonadLogger m) => Entity Answer -> SqlPersistT m ()
addAnswer' a = insertKey (entityKey a) (entityVal a)

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
