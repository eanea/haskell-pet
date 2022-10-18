{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}   
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module User.Repository.UserEntity where

import Database.Persist.Sql (SqlPersistT, runMigration, fromSqlKey)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Data.Text (Text)
import Data.Int (Int64)
import Database.Persist.Class.PersistEntity (Entity(..))
import Servant.Auth.Server (ToJWT, FromJWT)

import User.Models.User
share
    [ mkPersist sqlSettings
    , mkMigrate "migrateAll"
    ] [persistLowerCase|
UserEntity json sql=user
    name Text
    email Text
    deriving Eq Show
|]

instance ToJWT UserEntity
instance FromJWT UserEntity

doMigrations :: SqlPersistT IO ()
doMigrations = do
    runMigration migrateAll

toDto :: Entity UserEntity -> User
toDto userEntity = User {id = id', name = name', email = email'}
    where
        id' = fromSqlKey (entityKey userEntity)
        name' = userEntityName record
        email' = userEntityEmail record
        record = (entityVal userEntity)

toDtoWithId :: Int64 -> UserEntity -> User
toDtoWithId userId record = User {id = id', name = name', email = email'}
    where
        id' = userId
        name' = userEntityName record
        email' = userEntityEmail record