
module Kitty.Db.Model where

import Database.PostgreSQL.Simple.FromRow (FromRow, RowParser, fromRow, field)

import Kitty.Db.Geom (Geometry)

data ImageInfo = ImageInfo
  { imageId :: Int64
  , imageFileName :: FilePath
  , imageGeom :: Geometry
  } deriving Show

instance FromRow ImageInfo where
  fromRow :: RowParser ImageInfo
  fromRow =
    ImageInfo
      <$> field
      <*> field
      <*> field
