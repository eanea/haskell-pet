{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData, OverloadedStrings #-}

module DB (runDb, makePool, runDbWithIsolation) where 

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)

import Database.Persist.Sql (SqlPersistT, runSqlPool, runSqlPoolWithIsolation)
import Database.Persist.Postgresql (ConnectionPool, ConnectionString, IsolationLevel, createPostgresqlPool)

import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

import Config (Config, dbPool)
import Config (DBConfig(..))

makePool:: (MonadUnliftIO m, MonadLoggerIO m) => DBConfig -> m ConnectionPool
makePool dbConfig = createPostgresqlPool (connStr dbConfig) poolSize
    where poolSize = configPoolSize dbConfig

connStr :: DBConfig -> ConnectionString
connStr dbConfig = encodeUtf8 . T.pack $ unwords [host', dbname', user', password', port']
    where
        host' = "host=" ++ host dbConfig
        dbname' = "dbname=" ++ dbname dbConfig
        user' = "user=" ++ user dbConfig
        password' = "password=" ++ password dbConfig
        port' = "port=" ++ (show $ port dbConfig)

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks dbPool
    liftIO $ runSqlPool query pool

runDbWithIsolation :: (MonadReader Config m, MonadIO m) => IsolationLevel -> SqlPersistT IO b -> m b
runDbWithIsolation level query = do
    pool <- asks dbPool
    liftIO $ runSqlPoolWithIsolation query pool level