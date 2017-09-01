
module RIO where

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

