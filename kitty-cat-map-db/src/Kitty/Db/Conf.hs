
module Kitty.Db.Conf
  ( DbConf(..)
  , HasDbConf(..)
  , Pool
  , Connection
  , HasPool(..)
  , PgConnStr(..)
  , mkDbConf
  ) where

import Control.Lens (Lens', lens)
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)

data DbConf = DbConf
  { dbConfPool :: Pool Connection
  }

class HasDbConf s where
  dbConf :: Lens' s DbConf

instance HasDbConf DbConf where
  dbConf :: Lens' DbConf DbConf
  dbConf = id

class HasPool s where
  pool :: Lens' s (Pool Connection)

instance HasPool (Pool Connection) where
  pool :: Lens' (Pool Connection) (Pool Connection)
  pool = id

instance HasPool DbConf where
  pool :: Lens' DbConf (Pool Connection)
  pool = lens dbConfPool (\s a -> s { dbConfPool = a })

newtype PgConnStr = PgConnStr
  { unPGConnStr :: ByteString
  } deriving (IsString, Show)

mkDbConf
  :: MonadIO m
  => PgConnStr -> m DbConf
mkDbConf (PgConnStr connStr) = do
  connPool <- liftIO $ createPool (connectPostgreSQL connStr) close 1 60 10
  pure $ DbConf {dbConfPool = connPool}
