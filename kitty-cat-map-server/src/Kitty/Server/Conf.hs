
module Kitty.Server.Conf where

import Control.Lens (Lens', lens)
import Network.Wai.Handler.Warp (Port)
import System.ReadEnvVar (lookupEnvDef, readEnvDef)

import Kitty.Db
       (Connection, DbConf, HasDbConf(dbConf), HasPool(pool), PgConnStr,
        Pool, mkDbConf)
import Kitty.Img
       (HasImgConf(imgConf), HasImgDir(imgDir), HasImgUrl(imgUrl),
        ImgConf, ImgDir, ImgUrl, mkImgConf)

data ServerConf = ServerConf
  { serverConfDbConf :: !DbConf
  , serverConfImgConf :: !ImgConf
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

instance HasImgConf ServerConf where
  imgConf :: Lens' ServerConf ImgConf
  imgConf = lens serverConfImgConf (\s a -> s {serverConfImgConf = a})

instance HasImgDir ServerConf where
  imgDir :: Lens' ServerConf ImgDir
  imgDir = imgConf . imgDir

instance HasImgUrl ServerConf where
  imgUrl :: Lens' ServerConf ImgUrl
  imgUrl = imgConf . imgUrl

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
  serverPort <- readEnvDef "PORT" 8090
  serverImgDir <- lookupEnvDef "KITTY_IMG_DIR" ".images/"
  serverImgUrl <-
    lookupEnvDef "KITTY_IMG_URL" $ "http://localhost:8090/image/"
  pgConnStr <-
    lookupEnvDef
      "KITTY_DB_CONN_STR"
      "postgres://kitty-cat-map:foobar@localhost:5432/kitty-cat-map"
  mkServerConf serverImgDir serverImgUrl serverPort pgConnStr

mkServerConf :: MonadIO m => ImgDir -> ImgUrl -> Port -> PgConnStr -> m ServerConf
mkServerConf imgDir' imgUrl' serverConfPort pgConnStr = do
  dbConfig <- mkDbConf pgConnStr
  imgConfig <- mkImgConf imgDir' imgUrl'
  pure $
    ServerConf
    { serverConfDbConf = dbConfig
    , serverConfImgConf = imgConfig
    , serverConfPort
    }
