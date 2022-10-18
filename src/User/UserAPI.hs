{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module User.UserAPI (UserAPI, ProtectedAPI, PublicAPI, userApi) where

import Data.Int (Int64)
import Servant
import Servant.Auth.Server as SAS

import User.Models.CreateUser
import User.Models.User
import User.Repository.UserEntity (UserEntity)

type PublicAPI = Get '[JSON] [User]
      :<|> Capture "id" Int64 :> Get '[JSON] User
      :<|> ReqBody '[JSON] CreateUser :> Post '[JSON] Int64

type Protected = Capture "id" Int64 :> Verb 'DELETE 204 '[JSON] NoContent

type ProtectedAPI auths = (SAS.Auth auths UserEntity :> Protected)

type UserAPI auths = "users" :> (PublicAPI :<|> ProtectedAPI auths)

userApi :: Proxy (UserAPI '[JWT])
userApi = Proxy