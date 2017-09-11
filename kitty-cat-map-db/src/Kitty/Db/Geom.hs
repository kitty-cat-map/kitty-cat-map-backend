{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kitty.Db.Geom where

import Data.Aeson
       (FromJSON, ToJSON, Value, (.:), (.=), parseJSON, object, toJSON,
        withObject)
import Data.Aeson.Types (Parser)
import qualified Data.Binary.Builder as Binary
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow
       (FromRow, RowParser, field, fromRow)
import Database.PostgreSQL.Simple.ToField
       (Action(Plain), ToField, toField)
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)

newtype Lat = Lat { unLat :: Double }
  deriving (Eq, FromField, FromJSON, Num, Read, Show, ToField, ToJSON)

newtype Lon = Lon { unLon :: Double }
  deriving (Eq, FromField, FromJSON, Num, Read, Show, ToField, ToJSON)

data Geom = Geom
  { geomLat :: {-# UNPACK #-}!Lat
  , geomLon :: {-# UNPACK #-}!Lon
  } deriving (Eq, Read, Show)

instance FromRow Geom where
  fromRow :: RowParser Geom
  fromRow = Geom <$> field <*> field

instance ToRow Geom where
  toRow :: Geom -> [Action]
  toRow (Geom lat lon) =
    let latBS = fromPlain $ toField lat
        lonBS = fromPlain $ toField lon
    in [Plain $ "ST_SetSRID(ST_POINT(" <> latBS <> ", " <> lonBS <> "), 4326)"]
    where
      fromPlain :: Action -> Binary.Builder
      fromPlain (Plain builder) = builder
      fromPlain _ = error "Actions of other types are not supported."

instance FromJSON Geom where
  parseJSON :: Value -> Parser Geom
  parseJSON = withObject "Geom" $ \o -> do
    lat <- o .: "lat"
    lon <- o .: "lon"
    pure $ Geom lat lon

instance ToJSON Geom where
  toJSON :: Geom -> Value
  toJSON (Geom lat lon) = object ["lat" .= lat, "lon" .= lon]
