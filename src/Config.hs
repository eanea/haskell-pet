{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
module Config (Config(..), DBConfig(..)) where

import Database.Persist.Postgresql (ConnectionPool)
import Network.Wai.Handler.Warp (Port)
import Servant.Server (Context)
import Servant.Auth.Server as SAS

data Config = Config {
  dbPool :: ConnectionPool,
  appPort :: Port,
  jwtCfg :: JWTSettings,
  authCtx :: Context '[SAS.CookieSettings, SAS.JWTSettings]
  }

data DBConfig = DBConfig {
  host :: String,
  dbname :: String,
  user :: String,
  password :: String,
  port:: Int,
  configPoolSize :: Int
  }