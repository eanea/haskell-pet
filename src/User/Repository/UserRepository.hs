module User.Repository.UserRepository(findById, getAll, createUser, deleteById) where

import Database.Persist.Postgresql (SqlPersistT)
import Database.Persist.Postgresql (Entity(..), toSqlKey)
import Database.Persist.Postgresql (insert, selectList, get, delete)
import Data.Int (Int64)

import User.Repository.UserEntity as UserEntity
import User.Models.CreateUser (CreateUser(..))
import qualified User.Models.User as U (User(..))

findById :: Int64 -> SqlPersistT IO (Maybe (U.User))
findById userId = do
    userEntity <- get (toUserId userId)
    return $ (toDtoWithId userId) <$> userEntity

toUserId :: Int64 -> UserEntityId
toUserId = toSqlKey

getAll :: SqlPersistT IO [Entity UserEntity]
getAll = selectList [] []

createUser :: CreateUser -> SqlPersistT IO (Key UserEntity)
createUser user = insert user'
    where user' = UserEntity {userEntityName = name user, userEntityEmail = email user}

deleteById :: Int64 -> SqlPersistT IO ()
deleteById userId = delete (toUserId userId)
