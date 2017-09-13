{-# LANGUAGE TemplateHaskell #-}

module Kitty.Server.Api where

import Control.Lens (view)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Attoparsec.Text (maybeResult, parse)
import Data.Attoparsec.Time (utcTime)
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
       (Get, JSON, Post, ReqBody, Server, ServerT, (:>), (:<|>)((:<|>)), serve)
import qualified Servant as Servant
import Servant.Checked.Exceptions
       (Envelope, Throws, pureErrEnvelope, pureSuccEnvelope)
import Servant.Multipart
       (FileData(fdFilePath), FromMultipart(fromMultipart),
        MultipartData(files), MultipartForm, lookupInput)
import Servant.Utils.Enter ((:~>)(NT), enter)

import Kitty.Db
       (Geom(Geom), HasPool, ImgInfo, ImgInfo'(ImgInfo), ImgInfoKey, Lat,
        Lon, Offset, dbCreateImage, dbFindImages, mkLat, mkLon)
import Kitty.Server.Conf (ServerConf, mkServerConfEnv, port)
import Kitty.Server.Img (HasImgDir, ImgErr, copyImg, createImgDir)

---------
-- API --
---------

type Api = ImgApi :<|> SearchApi

type ImgApi = "image" :> PostImage

type PostImage =
  MultipartForm PostImgForm :>
  Throws ImgErr :>
  Post '[JSON] ImgInfoKey

type SearchApi = "search" :> GetSearchImg

type GetSearchImg =
  "image" :>
  ReqBody '[JSON] GetSearchImgForm :>
  Throws Void :>
  Get '[JSON] [ImgInfo]

---------------------------------
-- JSON Input and Output Types --
---------------------------------

data PostImgForm = PostImgForm
  { filename :: FilePath
  , date :: UTCTime
  , geom :: Geom
  }

parseDate :: Text -> Maybe UTCTime
parseDate = maybeResult . parse utcTime

instance FromMultipart PostImgForm where
  fromMultipart :: MultipartData -> Maybe PostImgForm
  fromMultipart multi = do
    tmpFile <- fdFilePath <$> listToMaybe (files multi)
    date <- parseDate =<< lookupInput "date" multi
    lat <- mkLat =<< readMay =<< lookupInput "lat" multi
    lon <- mkLon =<< readMay =<< lookupInput "lon" multi
    pure $ PostImgForm tmpFile date (Geom lat lon)

data GetSearchImgForm = GetSearchImgForm
  { minLat :: Lat
  , maxLat :: Lat
  , minLon :: Lon
  , maxLon :: Lon
  , page :: Offset
  }

$(deriveJSON defaultOptions ''GetSearchImgForm)

data ImgSearchRes = ImgSearchRes
  { id :: ImgInfoKey
  , url :: Text
  , date :: UTCTime
  , geom :: Geom
  } deriving Show

--------------
-- Handlers --
--------------

serverRoot
  :: ( HasImgDir r
     , HasPool r
     , MonadBaseControl IO m
     , MonadCatch m
     , MonadIO m
     , MonadReader r m
     , MonadThrow m
     )
  => ServerT Api m
serverRoot = imgApi :<|> searchApi

imgApi
  :: ( HasImgDir r
     , HasPool r
     , MonadBaseControl IO m
     , MonadCatch m
     , MonadIO m
     , MonadReader r m
     , MonadThrow m
     )
  => ServerT ImgApi m
imgApi = postImage

postImage
  :: ( HasImgDir r
     , HasPool r
     , MonadBaseControl IO m
     , MonadCatch m
     , MonadIO m
     , MonadReader r m
     , MonadThrow m
     )
  => PostImgForm -> m (Envelope '[ImgErr] ImgInfoKey)
postImage PostImgForm{filename, date, geom} = do
  eitherImg <- copyImg filename
  case eitherImg of
    Left imgErr -> pureErrEnvelope imgErr
    Right imgPath -> do
      let imageInfo = ImgInfo () imgPath date geom
      imageId <- dbCreateImage imageInfo
      pureSuccEnvelope imageId

searchApi
  :: (HasPool r, MonadBaseControl IO m, MonadIO m, MonadReader r m)
  => ServerT GetSearchImg m
searchApi = getSearchImage

getSearchImage
  :: (HasPool r, MonadBaseControl IO m, MonadIO m, MonadReader r m)
  => GetSearchImgForm -> m (Envelope '[Void] [ImgInfo])
getSearchImage GetSearchImgForm{minLat, maxLat, minLon, maxLon, page} = do
  images <- dbFindImages minLat maxLat minLon maxLon page 20
  pureSuccEnvelope images

--------------------------
-- Application and main --
--------------------------

app :: ServerConf -> Application
app config = serve (Proxy :: Proxy Api) apiServer
  where
    apiServer :: Server Api
    apiServer = enter naturalTrans serverRoot

    naturalTrans :: RIO ServerConf :~> Servant.Handler
    naturalTrans = NT transformation

    transformation :: forall a . RIO ServerConf a -> Servant.Handler a
    transformation = runRIO config

defaultMain :: MonadIO m => m ()
defaultMain = do
  conf <- mkServerConfEnv
  let port' = view port conf
  runReaderT createImgDir conf
  putStrLn $ "kitty-cat-map running on port " <> tshow port'
  liftIO . run port' $ app conf
