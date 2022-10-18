{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module User.Service.UserServer(userServer) where

import Database.Persist.Postgresql (fromSqlKey, IsolationLevel(..))
import Servant
import Control.Monad.Except (MonadIO)
import Data.Int (Int64)
import Servant.Auth.Server as SAS
import Database.Persist.Postgresql (SqlPersistT)

import AppT
import User.Models.User as User
import User.Models.CreateUser
import User.UserAPI
import User.Repository.UserEntity (UserEntity(..), toDto)
import DB (runDb, runDbWithIsolation)
import qualified User.Repository.UserRepository as UserRepo


publicServer :: MonadIO m => ServerT PublicAPI (AppT m)
publicServer = getAllUsers :<|> getUserById :<|> createUser

getAllUsers :: MonadIO m => AppT m [User]
getAllUsers = do
    users <- runDb (UserRepo.getAll)
    return $ map toDto users

getUserById :: MonadIO m => Int64 -> AppT m User
getUserById userId = do
    maybeUser <- runDb (UserRepo.findById userId)
    case maybeUser of
         Nothing ->
            throwError err404
         Just user ->
            return $ user

createUser :: MonadIO m => CreateUser -> AppT m Int64
createUser user = do
    newUser <- runDb (UserRepo.createUser user)
    return $ fromSqlKey newUser

deleteUser :: MonadIO m => UserEntity -> Int64 -> AppT m NoContent
deleteUser userEntity userId = do
    maybeDeleted <- runDbWithIsolation Serializable (deleteQuery userEntity userId)
    case maybeDeleted of
         Nothing ->
            throwError err401
         Just _ ->
            return NoContent

deleteQuery :: UserEntity -> Int64 -> SqlPersistT IO (Maybe Int64)
deleteQuery userEntity userId = do
    maybeUser <- UserRepo.findById userId
    case maybeUser of
         Nothing -> return Nothing
         Just user ->
            if (User.name user) == (userEntityName userEntity)
                then (UserRepo.deleteById userId) >> (return $ Just userId)
                else return Nothing

protectedServer :: MonadIO m => ServerT (ProtectedAPI auths) (AppT m)
protectedServer (SAS.Authenticated user) = (deleteUser user)
protectedServer _ = throwAll err401

-- | The server that runs the UserAPI
userServer :: MonadIO m => ServerT (UserAPI auths) (AppT m)
userServer = publicServer :<|> protectedServer