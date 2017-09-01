{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

module Prelude
  ( module X
  , module Prelude
  ) where

import ClassyPrelude as X

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

runRIO :: RIO env a -> env -> IO a
runRIO (RIO r) = runReaderT r
