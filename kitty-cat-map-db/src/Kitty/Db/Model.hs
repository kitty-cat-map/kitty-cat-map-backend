
module Kitty.Db.Model where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow, RowParser, fromRow, field)

import Kitty.Db.Geom (Geom)

newtype ImageInfoKey = ImageInfoKey { unImageInfoKey :: UUID }
  deriving (Eq, FromField, FromJSON, Read, Show, ToJSON)

data ImageInfo' key = ImageInfo
  { imageId :: key
  , imageFileName :: FilePath
  , imageDate :: UTCTime
  , imageGeom :: Geom
  } deriving Show

type ImageInfo = ImageInfo' ImageInfoKey

type ImageInfoData = ImageInfo' ()

instance FromField key => FromRow (ImageInfo' key) where
  fromRow :: RowParser (ImageInfo' key)
  fromRow =
    ImageInfo
      <$> field
      <*> field
      <*> field
      <*> fromRow
