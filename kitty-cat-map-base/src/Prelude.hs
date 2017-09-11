{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

module Prelude
  ( module X
  , module Prelude
  ) where

import ClassyPrelude as X
import Control.Monad.Except as X (MonadError(..), runExceptT)
import Control.Monad.Logger

-- import UnliftIO

newtype RIO env a = RIO
  { unRIO :: ReaderT env IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader env
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadBase IO
             )

runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO env (RIO (ReaderT f)) = liftIO $ f env

-- FIXME move into monad-logger itself
type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()
class HasLogFunc env where
  getLogFunc :: env -> LogFunc
instance HasLogFunc LogFunc where
  getLogFunc = id

instance HasLogFunc env => MonadLogger (RIO env) where
  monadLoggerLog a b c d = do
    f <- asks getLogFunc
    liftIO $ f a b c $ toLogStr d

instance HasLogFunc env => MonadLoggerIO (RIO env) where
  askLoggerIO = asks getLogFunc

-- instance MonadUnliftIO (RIO env) where
--   askUnliftIO =
--     RIO $ ReaderT $ \r ->
--       withUnliftIO $ \u ->
--         pure (UnliftIO (unliftIO u . flip runReaderT r . unRIO))

instance MonadBaseControl IO (RIO env) where
  type StM (RIO env) a = a
  liftBaseWith f = RIO $ ReaderT $ \r -> f $ runRIO r
  restoreM = pure
