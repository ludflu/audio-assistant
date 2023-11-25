{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ChatLogger where

import Database.Persist.Postgresql

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Person
    name String
    age Int
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
  runMigration migrateAll
  insert $ Person "Alice" 25
  alice <- selectList [PersonName ==. "Alice"] []
  liftIO $ print alice