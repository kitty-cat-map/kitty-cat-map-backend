{-# LANGUAGE TemplateHaskell #-}

module Kitty.Db.Model where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow, RowParser, fromRow, field)

import Kitty.Db.Geom (Geom)

newtype ImgInfoKey = ImgInfoKey { unImgInfoKey :: UUID }
  deriving (Eq, FromField, FromJSON, Read, Show, ToJSON)

data ImgInfo' key = ImgInfo
  { imageId :: key
  , imageFilename :: FilePath
  , imageDate :: UTCTime
  , imageGeom :: Geom
  } deriving Show

$(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''ImgInfo')

type ImgInfo = ImgInfo' ImgInfoKey

type ImgInfoData = ImgInfo' ()

instance FromField key => FromRow (ImgInfo' key) where
  fromRow :: RowParser (ImgInfo' key)
  fromRow =
    ImgInfo
      <$> field
      <*> field
      <*> field
      <*> fromRow
