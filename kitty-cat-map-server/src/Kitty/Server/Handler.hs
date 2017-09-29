{-# LANGUAGE TemplateHaskell #-}

module Kitty.Server.Handler
  ( module Kitty.Server.Handler
  , module Kitty.Api
  )where

import Control.Lens (view)
import Network.Wai (Application)
import Servant (ServerT, (:<|>)((:<|>)))
import Servant.Checked.Exceptions
       (Envelope, pureErrEnvelope, pureSuccEnvelope)
import Servant.RawM (serveDirectoryWebApp)

import Kitty.Api (Api, ImgApi, ImgRes(ImgRes, id, date, geom, url), PostImgForm(PostImgForm, date, filename, geom), SearchApi)
import Kitty.Db
       (HasPool, ImgInfo,
        ImgInfo'(ImgInfo, imgId, imgFilename, imgDate, imgGeom),
        ImgInfoKey, Lat, Lon, Offset, dbCreateImage, dbFindImages)
import Kitty.Img
       (HasImgDir, HasImgUrl, ImgDir(ImgDir), ImgErr, copyImg,
        imgDir, imgFilenameToUrl)

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
imgApi = postImage :<|> getImageFile

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

getImageFile :: (HasImgDir r, MonadReader r m) => m Application
getImageFile = do
  ImgDir dir <- view imgDir
  serveDirectoryWebApp dir

searchApi
  :: (HasImgUrl r, HasPool r, MonadBaseControl IO m, MonadIO m, MonadReader r m)
  => ServerT SearchApi m
searchApi = getSearchImage

getSearchImage
  :: (HasImgUrl r, HasPool r, MonadBaseControl IO m, MonadIO m, MonadReader r m)
  => Lat -> Lat -> Lon -> Lon -> Offset -> m (Envelope '[] [ImgRes])
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
