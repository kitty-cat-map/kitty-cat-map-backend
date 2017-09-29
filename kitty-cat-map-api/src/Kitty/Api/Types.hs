{-# LANGUAGE TemplateHaskell #-}

module Kitty.Api.Types where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Attoparsec.Text (maybeResult, parse)
import Data.Attoparsec.Time (utcTime)
import Servant.Multipart
       (FileData(fdFilePath), FromMultipart(fromMultipart),
        MultipartData(files), lookupInput)

import Kitty.Db (Geom(Geom), ImgInfoKey, mkLat, mkLon)

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
