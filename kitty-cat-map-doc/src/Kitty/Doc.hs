{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kitty.Doc where

import Control.Lens ((<>~), view)
import Data.Maybe (fromJust)
import Data.Function ((&))
import Data.Time.Format (iso8601DateFormat, parseTimeOrError)
import qualified Data.UUID as UUID
import Servant.API (Capture, (:>))
import Servant.Docs
       (API, Action, DocCapture(DocCapture), DocNote(DocNote),
        DocOptions, Endpoint, HasDocs(docsFor),
        ToCapture(toCapture), ToSample(toSamples), docs, markdown,
        maxSamples, notes)
import Servant.Multipart
       (FileData(FileData), Input(Input), MultipartData(MultipartData),
        MultipartForm)

import Kitty.Api
       (Api, ImgRes(ImgRes, id, url, date, geom), PostImgForm)
import Kitty.Db (ImgInfoKey(ImgInfoKey), Lat, Lon, Offset, unsafeMkGeom)
import Kitty.Img
       (ImgErr(ImgErrHashErr, ImgErrImgTypeErr, ImgErrNotImg,
               ImgErrCouldNotCopy))

class ToMultipartSample a where
  toMultipartSamples :: Proxy a -> [(Text, MultipartData)]

instance ToMultipartSample PostImgForm where
  toMultipartSamples :: Proxy PostImgForm -> [(Text, MultipartData)]
  toMultipartSamples Proxy =
    [ ( "normal image upload"
      , MultipartData
          [ Input "date" "2017-09-10 08:23 Z"
          , Input "lat" "-33"
          , Input "lon" "-100"
          ]
          [ FileData
              "file"
              "temp-cat-image.jpg"
              "image/jpeg"
              "/tmp/tmppath.file"
          ]
      )
    ]

multipartInputToItem :: Input -> Text
multipartInputToItem (Input name val) =
  "        -   *" <> name <> "*: " <> "`" <> val <> "`"

multipartFileToItem :: FileData -> Text
multipartFileToItem (FileData name _ contentType _) =
  "        -   *" <> name <> "*, content-type: " <> "`" <> contentType <> "`"

multipartSampleToDesc :: (Text, MultipartData) -> [Text]
multipartSampleToDesc (desc, MultipartData inputs files) =
  [ "-   " <> desc
  , "    -   textual inputs:"
  ] <>
  fmap multipartInputToItem inputs <>
  [ "    -   file inputs:" ] <>
  fmap multipartFileToItem files


toMultipartDescriptions :: ToMultipartSample a => Proxy a -> [[Text]]
toMultipartDescriptions proxy =
  fmap multipartSampleToDesc (toMultipartSamples proxy)

toMultipartNotes :: ToMultipartSample a => Int -> Proxy a -> DocNote
toMultipartNotes maxSamples' proxy =
  let sampleLines = take maxSamples' $ toMultipartDescriptions proxy
      body =
        [ "This endpoint takes multipart/form-data requests.  The following are sample "
        , "requests:"
        , ""
        ] <> fold sampleLines
  in DocNote "Multipart Request Samples" $ fmap unpack body

instance HasDocs api => HasDocs (MultipartForm PostImgForm :> api) where
  docsFor
    :: Proxy (MultipartForm PostImgForm :> api)
    -> (Endpoint, Action)
    -> DocOptions
    -> API
  docsFor _ (endpoint, action) opts =
    let newAction =
          action
            & notes <>~
                [ toMultipartNotes
                    (view maxSamples opts)
                    (Proxy :: Proxy PostImgForm)
                ]
    in docsFor (Proxy :: Proxy api) (endpoint, newAction) opts

-- instance (ToSample a, AllMimeRender (ct ': cts) a, HasDocs api)
--       => HasDocs (ReqBody (ct ': cts) a :> api) where

--   docsFor Proxy (endpoint, action) opts@DocOptions{..} =
--     docsFor subApiP (endpoint, action') opts

--     where subApiP = Proxy :: Proxy api
--           action' :: Action
--           action' = action & rqbody .~ take _maxSamples (sampleByteStrings t p)
--                            & rqtypes .~ allMime t
--           t = Proxy :: Proxy (ct ': cts)
--           p = Proxy :: Proxy a

instance ToCapture (Capture "maxLat" Lat) where
  toCapture :: Proxy (Capture "maxLat" Lat) -> DocCapture
  toCapture _ = DocCapture "minLat" "maximum latitude"

instance ToCapture (Capture "minLat" Lat) where
  toCapture :: Proxy (Capture "minLat" Lat) -> DocCapture
  toCapture _ = DocCapture "minLat" "minimum latitude"

instance ToCapture (Capture "maxLon" Lon) where
  toCapture :: Proxy (Capture "maxLon" Lon) -> DocCapture
  toCapture _ = DocCapture "maxLon" "maximum longitude"

instance ToCapture (Capture "minLon" Lon) where
  toCapture :: Proxy (Capture "minLon" Lon) -> DocCapture
  toCapture _ = DocCapture "minLon" "minimum longitude"

instance ToCapture (Capture "offset" Offset) where
  toCapture :: Proxy (Capture "offset" Offset) -> DocCapture
  toCapture _ = DocCapture "offset" "paging offset"

instance ToSample ImgErr where
  toSamples :: Proxy ImgErr -> [(Text, ImgErr)]
  toSamples _ =
    [ ("Error when trying to take the hash of the temporary file", ImgErrHashErr)
    , ("Error when reading the first few bytes of the temporary file to find its magic number", ImgErrImgTypeErr)
    , ("The input file did not have a correct magic number.", ImgErrNotImg)
    , ("Error when trying to copy the temporary file to the images directory.", ImgErrCouldNotCopy)
    ]

sampleImgInfoKey1 :: ImgInfoKey
sampleImgInfoKey1 =
  ImgInfoKey . fromJust $ UUID.fromString "0359141a-9ce8-4ca9-b93c-d9d017ece471"

sampleImgInfoKey2 :: ImgInfoKey
sampleImgInfoKey2 =
  ImgInfoKey . fromJust $ UUID.fromString "63f1f6d9-4cbe-4b49-a57a-54356defb55c"

instance ToSample ImgInfoKey where
  toSamples :: Proxy ImgInfoKey -> [(Text, ImgInfoKey)]
  toSamples _ = [("sample uuid key", sampleImgInfoKey1)]

instance ToSample ImgRes where
  toSamples :: Proxy ImgRes -> [(Text, ImgRes)]
  toSamples _ =
    [ ( "sample image 1"
      , ImgRes
        { id = sampleImgInfoKey1
        , url =
          "http://localhost:8090/image/" <>
          "60475399b11663a107b06a188a795a1e02387535933bd9f5318fa01a1593a6d1.jpg"
        , date =
          parseTimeOrError
            True
            defaultTimeLocale
            (iso8601DateFormat (Just "%H:%M:%S"))
            ("2017-10-11T05:55:25")
        , geom = unsafeMkGeom 20 (-130)
        }
      )
    , ( "sample image 2"
      , ImgRes
        { id = sampleImgInfoKey2
        , url =
          "http://localhost:8090/image/" <>
          "1f63769f83fe0721f05cd410fccea76870ad60d12fedd6b41ee78da8989edbd8.jpg"
        , date =
          parseTimeOrError
            True
            defaultTimeLocale
            (iso8601DateFormat (Just "%H:%M:%S"))
            ("2016-05-01T10:11:11")
        , geom = unsafeMkGeom (-80) (-160)
        }
      )
    ]

defaultMain :: IO ()
defaultMain = putStrLn . pack . markdown $ docs (Proxy :: Proxy Api)
