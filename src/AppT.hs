{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppT (AppT, runApp) where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Servant.Server (ServerError)

import Config (Config)

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype (AppT) m a
    = AppT
    { runApp :: ReaderT Config (ExceptT ServerError m) a
    } deriving
    ( Functor, Applicative, Monad, MonadReader Config, MonadError ServerError
    , MonadIO
    )
