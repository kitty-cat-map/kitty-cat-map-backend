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
import Web.HttpApiData (FromHttpApiData)

newtype Lat = Lat { unLat :: Double }
  deriving (Eq, FromField, FromHttpApiData, Num, Read, Show, ToField, ToJSON)

instance FromJSON Lat where
  parseJSON :: Value -> Parser Lat
  parseJSON = maybe (fail "Lat out of range") pure . mkLat <=< parseJSON

latMin :: Double
latMin = -90

latMax :: Double
latMax = 90

mkLat :: Double -> Maybe Lat
mkLat lat
  | lat >= latMin && lat <= latMax = Just $ Lat lat
  | otherwise = Nothing

newtype Lon = Lon { unLon :: Double }
  deriving (Eq, FromField, FromHttpApiData, Num, Read, Show, ToField, ToJSON)

instance FromJSON Lon where
  parseJSON :: Value -> Parser Lon
  parseJSON = maybe (fail "Lon out of range") pure . mkLon <=< parseJSON

lonMin :: Double
lonMin = -180

lonMax :: Double
lonMax = 180

mkLon :: Double -> Maybe Lon
mkLon lon
  | lon >= lonMin && lon <= lonMax = Just $ Lon lon
  | otherwise = Nothing

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
