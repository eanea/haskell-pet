{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
module Swagger (SwaggerUI, swaggerDoc, schemaUiServer) where

import Control.Lens hiding ((.=))

import Data.Aeson (Value)
import Data.Swagger

import Servant
import Servant.Swagger.UI (SwaggerSchemaUI, SwaggerSchemaUI', swaggerSchemaUIServer)
import Servant.Swagger (HasSwagger, toSwagger)
import Servant.Auth.Swagger ()

-- this serves both: swagger.json and swagger-ui
type SwaggerUI = SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDoc :: HasSwagger api => Proxy api -> Swagger
swaggerDoc api = toSwagger (api)
    & info.title       .~ "Users API"
    & info.version     .~ "0.1"
    & info.description ?~ "Pet project"

schemaUiServer :: (Server api ~ Handler Value) => Swagger -> Server (SwaggerSchemaUI' dir api)
schemaUiServer = swaggerSchemaUIServer