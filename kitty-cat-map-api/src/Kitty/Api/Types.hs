{-# LANGUAGE TemplateHaskell #-}

module Kitty.Api.Types where

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
       (Envelope, NoThrow, Throws, pureErrEnvelope, pureSuccEnvelope)
import Servant.Multipart
       (FileData(fdFilePath), FromMultipart(fromMultipart),
        MultipartData(files), MultipartForm, lookupInput)
import Servant.RawM (RawM, serveDirectoryWebApp)
import Servant.Utils.Enter ((:~>)(NT), enter)

import Kitty.Db
       (Geom(Geom), HasPool, ImgInfo,
        ImgInfo'(ImgInfo, imgId, imgFilename, imgDate, imgGeom), ImgInfoKey,
        Lat, Lon, Offset, dbCreateImage, dbFindImages, mkLat, mkLon)
import Kitty.Img
       (HasImgDir, HasImgUrl, ImgDir(ImgDir), ImgErr, copyImg,
        createImgDir, imgDir, imgFilenameToUrl)
import Kitty.Server.Conf (ServerConf, mkServerConfEnv, port)

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
