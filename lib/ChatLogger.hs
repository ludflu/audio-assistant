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
import Data.Maybe (listToMaybe, maybeToList)
import Data.Sequence.Internal.Sorting (Queue (Q))
import Data.Time (UTCTime)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Database.Esqueleto.Experimental
  ( BackendKey (SqlBackendKey),
    ConnectionPool,
    Entity (entityKey, entityVal),
    PersistEntity (Key),
    PersistQueryRead (selectFirst),
    PersistStoreRead (get),
    PersistStoreWrite (insert, insertKey, insert_),
    SqlExpr,
    SqlPersistT,
    SqlReadT,
    desc,
    from,
    limit,
    orderBy,
    runMigration,
    runSqlPersistMPool,
    select,
    table,
    val,
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

addAnswer :: Maybe ConnectionPool -> Answer -> IO ()
addAnswer dbPool answer = mapM_ (runSqlPersistMPool (addAnswer' answer)) dbPool

addQuery :: Maybe ConnectionPool -> Query -> IO (Maybe (Key Query))
addQuery dbPool query = mapM (runSqlPersistMPool (addQuery' query)) dbPool

getAnswersForLastQuestion :: Maybe ConnectionPool -> IO (Maybe [Answer])
getAnswersForLastQuestion = mapM (runSqlPersistMPool getAnswersForLastQuestion')

addQuery' :: (MonadIO m, MonadLogger m) => Query -> SqlPersistT m (Key Query)
addQuery' = insert

addAnswer' :: (MonadIO m, MonadLogger m) => Answer -> SqlPersistT m ()
addAnswer' = insert_

getAnswers :: (MonadIO m, MonadLogger m) => SqlReadT m [Entity Answer]
getAnswers = select $ from $ table @Answer

getAnswersForQuery :: (MonadIO m, MonadLogger m) => Entity Query -> SqlReadT m [Entity Answer]
getAnswersForQuery query = select $ do
  a <- from $ table @Answer
  let qkey = entityKey query
  where_ ((a ^. AnswerParent) ==. val qkey)
  return a

getLastQuery :: (MonadIO m, MonadLogger m) => SqlReadT m (Maybe (Entity Query))
getLastQuery = do
  r <- select $ do
    q <- from $ table @Query
    orderBy [desc (q ^. QueryStamp)]
    limit 1
    return q
  return $ listToMaybe r

getAnswersForLastQuestion' :: (MonadIO m, MonadLogger m) => SqlReadT m [Answer]
getAnswersForLastQuestion' = do
  lastQuery <- getLastQuery
  answers <- mapM getAnswersForQuery lastQuery
  let result = concat $ maybeToList answers
  return $ map entityVal result