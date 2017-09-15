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
       (Capture, Get, JSON, Post, Server, ServerT, (:>), (:<|>)((:<|>)),
        serve)
import qualified Servant as Servant
import Servant.Checked.Exceptions
       (Envelope, Throws, pureErrEnvelope, pureSuccEnvelope)
import Servant.Multipart
       (FileData(fdFilePath), FromMultipart(fromMultipart),
        MultipartData(files), MultipartForm, lookupInput)
import Servant.Utils.Enter ((:~>)(NT), enter)

import Kitty.Db
       (Geom(Geom), HasPool, ImgInfo,
        ImgInfo'(ImgInfo, imgId, imgFilename, imgDate, imgGeom), ImgInfoKey,
        Lat, Lon, Offset, dbCreateImage, dbFindImages, mkLat, mkLon)
import Kitty.Img
       (HasImgDir, HasImgUrl, ImgErr, copyImg, createImgDir,
        imgFilenameToUrl)
import Kitty.Server.Conf (ServerConf, mkServerConfEnv, port)

---------
-- API --
---------

type Api = "v0" :> (ImgApi :<|> SearchApi)

type ImgApi = "image" :> PostImage

type PostImage =
  MultipartForm PostImgForm :>
  Throws ImgErr :>
  Post '[JSON] ImgInfoKey

type SearchApi = "search" :> GetSearchImg

type GetSearchImg =
  "image" :>
  Capture "minLat" Lat :>
  Capture "maxLat" Lat :>
  Capture "minLon" Lon :>
  Capture "maxLon" Lon :>
  Capture "offset" Offset :>
  Throws Void :>
  Get '[JSON] [ImgRes]

---------------------------------
-- JSON Input and Output Types --
---------------------------------

data PostImgForm = PostImgForm
  { filename :: FilePath
  , date :: UTCTime
  , geom :: Geom
  }

instance FromMultipart PostImgForm where
  fromMultipart :: MultipartData -> Maybe PostImgForm
  fromMultipart multi = do
    tmpFile <- fdFilePath <$> listToMaybe (files multi)
    date <- parseDate =<< lookupInput "date" multi
    lat <- mkLat =<< readMay =<< lookupInput "lat" multi
    lon <- mkLon =<< readMay =<< lookupInput "lon" multi
    pure $ PostImgForm tmpFile date (Geom lat lon)
    where
      parseDate :: Text -> Maybe UTCTime
      parseDate = maybeResult . parse utcTime

data ImgRes = ImgRes
  { id :: ImgInfoKey
  , url :: Text
  , date :: UTCTime
  , geom :: Geom
  } deriving Show

$(deriveJSON defaultOptions ''ImgRes)

--------------
-- Handlers --
--------------

serverRoot
  :: ( HasImgDir r
     , HasImgUrl r
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
  :: (HasImgUrl r, HasPool r, MonadBaseControl IO m, MonadIO m, MonadReader r m)
  => ServerT SearchApi m
searchApi = getSearchImage

getSearchImage
  :: (HasImgUrl r, HasPool r, MonadBaseControl IO m, MonadIO m, MonadReader r m)
  => Lat -> Lat -> Lon -> Lon -> Offset -> m (Envelope '[Void] [ImgRes])
getSearchImage minLat maxLat minLon maxLon offset = do
  imgs <- dbFindImages minLat maxLat minLon maxLon offset 20
  imgResults <- traverse imgToRes imgs
  pureSuccEnvelope imgResults

imgToRes :: (HasImgUrl r, MonadReader r m) => ImgInfo -> m ImgRes
imgToRes ImgInfo {imgId, imgFilename, imgDate, imgGeom} = do
  url' <- imgFilenameToUrl imgFilename
  pure $
    ImgRes
    { id = imgId
    , url = url'
    , date = imgDate
    , geom = imgGeom
    }

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
