
module Kitty.Db.Model where

import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow, RowParser, fromRow, field)

import Kitty.Db.Geom (Geometry)

newtype ImageInfoKey = ImageInfoKey { unImageInfoKey :: UUID }
  deriving (Eq, FromField, Read, Show)

instance FromRow ImageInfoKey where
  fromRow :: RowParser ImageInfoKey
  fromRow = ImageInfoKey <$> field

data ImageInfo' key = ImageInfo
  { imageId :: key
  , imageFileName :: FilePath
  , imageGeom :: Geometry
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
