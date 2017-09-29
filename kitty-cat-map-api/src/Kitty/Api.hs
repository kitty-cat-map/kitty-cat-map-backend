{-# LANGUAGE TemplateHaskell #-}

module Kitty.Api where

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

type Api = "v0" :> (ImgApi :<|> SearchApi)

type ImgApi = "image" :> (PostImage :<|> GetImageFile)

type PostImage =
  MultipartForm PostImgForm :>
  Throws ImgErr :>
  Post '[JSON] ImgInfoKey

type GetImageFile = RawM

type SearchApi = "search" :> GetSearchImg

type GetSearchImg =
  "image" :>
  Capture "minLat" Lat :>
  Capture "maxLat" Lat :>
  Capture "minLon" Lon :>
  Capture "maxLon" Lon :>
  Capture "offset" Offset :>
  NoThrow :>
  Get '[JSON] [ImgRes]
