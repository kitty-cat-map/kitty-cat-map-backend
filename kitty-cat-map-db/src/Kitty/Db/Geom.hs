{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kitty.Db.Geom
  ( module Database.Postgis
  ) where

import Data.Aeson
       (FromJSON, ToJSON, Value, (.:), (.=), parseJSON, object, toJSON,
        withObject)
import Data.Aeson.Types (Parser)
import Data.Binary.Builder (fromLazyByteString)
import Data.Binary.Get (runGetOrFail)
import Database.PostgreSQL.Simple.FromField
       (Conversion, Field, FromField,
        ResultError(ConversionFailed, UnexpectedNull), fromField,
        returnError)
import Database.PostgreSQL.Simple.ToField
       (Action(Plain), ToField, toField)
import Database.Postgis (Geometry(GeoPoint), Point(Point, _x, _y), writeGeometry)
import Database.Postgis.Serialize (getGeometry)

instance FromField Geometry where
  fromField :: Field -> Maybe ByteString -> Conversion Geometry
  fromField field (Just bs) =
    case runGetOrFail getGeometry (fromStrict bs) of
      Left (_, _, msg) ->
        let errMsg = "could not convert Geometry from db: " <> msg
        in returnError ConversionFailed field errMsg
      Right (_, _, geom) -> pure geom
  fromField field Nothing = returnError UnexpectedNull field ""

instance ToField Geometry where
  toField  =  Plain . fromLazyByteString . writeGeometry

instance FromJSON Geometry where
  parseJSON :: Value -> Parser Geometry
  parseJSON = withObject "Geometry" $ \o -> do
    lat <- o .: "lat"
    lon <- o .: "lon"
    pure $ GeoPoint (Just 4326) (Point lat lon Nothing Nothing)

instance ToJSON Geometry where
  toJSON :: Geometry -> Value
  toJSON (GeoPoint _ Point{_x, _y}) = object ["lat" .= _y, "lon" .= _x]
  toJSON _ = error "Non GeoPoint values of Geometry are not supported."
