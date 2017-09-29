{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kitty.Doc where

import Servant.API
       (Capture, Get, JSON, Post, (:>), (:<|>))
import Servant.Docs
       (API, Action, DocCapture(DocCapture), DocOptions, Endpoint,
        HasDocs(docsFor), ToCapture(toCapture), ToSample(toSamples), docs,
        markdown, single)
import Servant.Checked.Exceptions (NoThrow, Throws)
import Servant.Multipart (MultipartForm)
import Servant.RawM (RawM)

import Kitty.Api
       (Api, ImgRes(ImgRes, id, url, date, geom),
        PostImgForm(PostImgForm, filename, date, geom))
import Kitty.Db (ImgInfoKey, Lat, Lon, Offset)
import Kitty.Img (ImgErr)


instance HasDocs api => HasDocs (MultipartForm PostImgForm :> api) where
  docsFor :: Proxy (MultipartForm PostImgForm :> api) -> (Endpoint, Action) -> DocOptions -> API
  docsFor _ (endpoint, action) _ = single endpoint action

instance HasDocs RawM where
  docsFor :: Proxy RawM -> (Endpoint, Action) -> DocOptions -> API
  docsFor = undefined

instance ToCapture (Capture "maxLat" Lat) where
  toCapture :: Proxy (Capture "maxLat" Lat) -> DocCapture
  toCapture = undefined

instance ToCapture (Capture "minLat" Lat) where
  toCapture :: Proxy (Capture "minLat" Lat) -> DocCapture
  toCapture = undefined

instance ToCapture (Capture "maxLon" Lon) where
  toCapture :: Proxy (Capture "maxLon" Lon) -> DocCapture
  toCapture = undefined

instance ToCapture (Capture "minLon" Lon) where
  toCapture :: Proxy (Capture "minLon" Lon) -> DocCapture
  toCapture = undefined

instance ToCapture (Capture "offset" Offset) where
  toCapture :: Proxy (Capture "offset" Offset) -> DocCapture
  toCapture = undefined

instance ToSample ImgErr where
  toSamples :: Proxy ImgErr -> [(Text, a)]
  toSamples = undefined

instance ToSample ImgInfoKey where
  toSamples :: Proxy ImgInfoKey -> [(Text, a)]
  toSamples = undefined

instance ToSample ImgRes where
  toSamples :: Proxy ImgRes -> [(Text, a)]
  toSamples = undefined

defaultMain :: IO ()
defaultMain = putStrLn . pack . markdown $ docs (Proxy :: Proxy Api)
