{-# LANGUAGE TemplateHaskell #-}

module Kitty.Api
  ( module Kitty.Api
  , module Kitty.Api.Types
  ) where

import Servant.API
       (Capture, Get, JSON, Post, (:>), (:<|>))
import Servant.Checked.Exceptions (NoThrow, Throws)
import Servant.Multipart (MultipartForm)
import Servant.RawM (RawM)

import Kitty.Api.Types
       (ImgRes(ImgRes, id, url, date, geom),
        PostImgForm(PostImgForm, filename, date, geom))
import Kitty.Db (ImgInfoKey, Lat, Lon, Offset)
import Kitty.Img (ImgErr)

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
