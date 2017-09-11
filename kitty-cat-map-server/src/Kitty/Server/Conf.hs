
module Kitty.Server.Conf where

import Control.Lens (Lens', lens)
import Network.Wai.Handler.Warp (Port)
import System.ReadEnvVar (lookupEnvDef, readEnvDef)

import Kitty.Db
       (Connection, DbConf, HasDbConf(dbConf), HasPool(pool), PgConnStr,
        Pool, mkDbConf)

newtype ImgDir = ImgDir { unImgDir :: FilePath }
  deriving (Eq, IsString, Read, Show)

data ServerConf = ServerConf
  { serverConfDbConf :: !DbConf
  , serverConfImgDir :: !ImgDir
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

class HasImgDir s where
  imgDir :: Lens' s ImgDir

instance HasImgDir ImgDir where
  imgDir :: Lens' ImgDir ImgDir
  imgDir = id

instance HasImgDir ServerConf where
  imgDir :: Lens' ServerConf ImgDir
  imgDir = lens serverConfImgDir (\s a -> s {serverConfImgDir = a})

instance HasPort ServerConf where
  port :: Lens' ServerConf Port
  port = lens serverConfPort (\s a -> s {serverConfPort = a})

mkServerConfEnv :: MonadIO m => m ServerConf
mkServerConfEnv = do
  serverPort <- readEnvDef "PORT" 8080
  serverImgDir <- lookupEnvDef "KITTY_IMG_DIR" ".images/"
  pgConnStr <-
    lookupEnvDef
      "KITTY_DB_CONN_STR"
      "postgres://kitty-cat-map:foobar@localhost:5432/kitty-cat-map"
  mkServerConf serverImgDir serverPort pgConnStr

mkServerConf :: MonadIO m => ImgDir -> Port -> PgConnStr -> m ServerConf
mkServerConf serverConfImgDir serverConfPort pgConnStr = do
  dbConfig <- mkDbConf pgConnStr
  pure $
    ServerConf {serverConfDbConf = dbConfig, serverConfImgDir, serverConfPort}
