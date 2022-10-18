{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Init(runApp) where

import Data.Typeable
import qualified Data.Text as Text
import Data.Text (Text, pack)
import Control.Monad.Logger
import Database.Persist.Postgresql (runSqlPool)
import Network.Wai (Application)
import Say
import Control.Exception.Safe
import Network.Wai.Handler.Warp (run)
import Data.Pool as Pool
import Servant.Server (Context(..))
import Servant.Auth.Server as SAS (JWTSettings, CookieSettings, makeJWT, generateKey, defaultJWTSettings, defaultCookieSettings)
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Run
import Config (Config(..), DBConfig(..))
import DB (makePool)
import User.Repository.UserEntity (UserEntity(..), doMigrations)

runApp :: IO ()
runApp = do
    withConfig $ \config ->  do
        say "acquired config"
        application <- mkApp config
            `finally` say "app is initialized"
        _ <- forkIO $ generateJWT config
        say "start server on http://localhost:8080"
        say "to generate JWT type 'name email'"
        run (appPort config) application
            `finally` (shutdownApp config >> say "server is closed")

-- | Takes care of cleaning up 'Config' resources
shutdownApp :: Config -> IO ()
shutdownApp cfg = do
    Pool.destroyAllResources (dbPool cfg)
    pure ()

mkApp :: Config -> IO Application
mkApp cfg = do
    say "initialize app"
    say "run migrations"
    bracket
        (say "starting to run migrations")
        (\_ -> say "migrations complete")
        $ \_ -> do
            say "actually running migrations"
            runSqlPool doMigrations (dbPool cfg) `catch` \(SomeException e) -> do
                say $ mconcat
                    [   "exception in doMigrations, type: "
                    , tshow (typeOf e)
                    , ", shown: "
                    , tshow e
                    ]
                throwIO e
            say "okay all done"
    say "making app"
    pure . appWithSwagger $ cfg

mkAuthCtx :: IO (SAS.JWTSettings, Context '[SAS.CookieSettings, SAS.JWTSettings])
mkAuthCtx = do
    myKey <- generateKey
    let jwtCfg' = defaultJWTSettings myKey
    let authCtx' = defaultCookieSettings :. jwtCfg' :. EmptyContext
    return (jwtCfg', authCtx')

withConfig :: (Config -> IO a) -> IO a
withConfig action = do
    !pool <- (runStderrLoggingT $ makePool dbConfig) `onException` say "exception in makePool"
    say $ "got pool "
    (jwtCfg', authCtx') <- mkAuthCtx
    action Config { dbPool = pool, appPort = 8080, jwtCfg = jwtCfg', authCtx = authCtx'}
    where dbConfig = DBConfig {
        host = "localhost",
        dbname = "pet",
        user = "postgres",
        password = "postgres",
        port = 5432,
        configPoolSize = 10
        }

generateJWT :: Config -> IO ()
generateJWT cfg =
  forever $ do
     xs <- words <$> getLine
     case xs of
       [name', email'] -> do
         etoken <- makeJWT (UserEntity (pack name') (pack email')) (jwtCfg cfg) Nothing
         case etoken of
           Left e -> putStrLn $ "Error generating token:t" ++ show e
           Right v -> putStrLn $ "New token:\t" ++ show v
       _ -> putStrLn "Expecting a name and email separated by spaces"

tshow :: Show a => a -> Text
tshow = Text.pack . show