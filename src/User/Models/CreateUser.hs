{-# LANGUAGE DeriveGeneric #-}
module User.Models.CreateUser where

import Data.Text (Text)

import Data.Aeson (FromJSON)
import Data.Swagger (ToSchema(..))
import GHC.Generics (Generic)

data CreateUser = CreateUser {
  name :: Text,
  email :: Text
} deriving (Show, Eq, Generic)

instance FromJSON CreateUser
instance ToSchema CreateUser