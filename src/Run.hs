{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (appWithSwagger) where

import Control.Monad.Reader (runReaderT)
import Servant
       ( (:<|>)((:<|>))
       , Proxy(Proxy)
       , EmptyAPI
       )
import Servant.Server
import Servant.Auth (JWT)
import Servant.Auth.Server (CookieSettings, JWTSettings)

import User.UserAPI (UserAPI, userApi)
import User.Service.UserServer (userServer)
import Config (Config(..))
import AppT (AppT(..))
import Swagger

appToServer :: Config -> Server (UserAPI '[JWT])
appToServer cfg = hoistServerWithContext userApi (Proxy :: Proxy '[CookieSettings, JWTSettings]) (convertApp cfg) userServer

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg



type AppAPI auths = UserAPI auths :<|> EmptyAPI

appApi :: Proxy (AppAPI '[JWT])
appApi = Proxy

type AppAPIWithSwagger auths = SwaggerUI :<|> AppAPI auths

appAPIWithSwagger :: Proxy (AppAPIWithSwagger '[JWT])
appAPIWithSwagger = Proxy

appWithSwagger :: Config -> Application
appWithSwagger cfg = serveWithContext appAPIWithSwagger authCtx' ((schemaUiServer (swaggerDoc appApi)) :<|> appToServer cfg :<|> emptyServer)
       where authCtx' = authCtx cfg