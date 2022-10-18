{-# LANGUAGE DeriveGeneric #-}
module User.Models.User where

import Data.Text (Text)
import Data.Int (Int64)

import Data.Aeson         (FromJSON, ToJSON)
import Data.Swagger (ToSchema(..))
import GHC.Generics (Generic)

data User = User {
  id :: Int64,
  name :: Text,
  email :: Text
} deriving (Show, Eq, Generic)

instance ToJSON User
instance FromJSON User

instance ToSchema User