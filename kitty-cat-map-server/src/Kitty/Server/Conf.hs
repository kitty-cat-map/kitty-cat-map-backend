
module Kitty.Server.Conf where

import Control.Lens (Lens', lens)
import Network.Wai.Handler.Warp (Port)
import System.ReadEnvVar (lookupEnvDef, readEnvDef)

import Kitty.Db
       (Connection, DbConf, HasDbConf(dbConf), HasPool(pool), PgConnStr,
        Pool, mkDbConf)

data ServerConf = ServerConf
  { serverConfDbConf :: !DbConf
  , serverConfPort :: {-# UNPACK #-} !Port
  }

class HasServerConf s where
  serverConf :: Lens' s ServerConf

instance HasServerConf ServerConf where
  serverConf :: Lens' ServerConf ServerConf
  serverConf = id

instance HasDbConf ServerConf where
  dbConf :: Lens' ServerConf DbConf
  dbConf = lens serverConfDbConf (\s a -> s {serverConfDbConf = a})

instance HasPool ServerConf where
  pool :: Lens' ServerConf (Pool Connection)
  pool = dbConf . pool

class HasPort s where
  port :: Lens' s Port

instance HasPort Port where
  port :: Lens' Port Port
  port = id

instance HasPort ServerConf where
  port :: Lens' ServerConf Port
  port = lens serverConfPort (\s a -> s {serverConfPort = a})

mkServerConfEnv :: MonadIO m => m ServerConf
mkServerConfEnv = do
  serverPort <- readEnvDef "PORT" 8080
  pgConnStr <-
    lookupEnvDef
      "KITTY_DB_CONN_STR"
      "postgres://kitty-cat-map:foobar@localhost:5432/kitty-cat-map"
  mkServerConf serverPort pgConnStr

mkServerConf :: MonadIO m => Port -> PgConnStr -> m ServerConf
mkServerConf serverPort pgConnStr = do
  dbConfig <- mkDbConf pgConnStr
  pure $ ServerConf {serverConfPort = serverPort, serverConfDbConf = dbConfig}
