
module Kitty.Db.Geom where

import Data.Binary.Get (runGetOrFail)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToField
       (Plain, ToField, fromByteString, toField)
import Postgis.DB (Geometry, getGeometry, writeGeometry)

instance ToField Geometry where
  toField  =  Plain . fromByteString . writeGeometry

instance FromField Geometry where
  fromField :: Field -> Maybe ByteString -> Conversion Geometry
  fromField field (Just bs) =
    case runGetOrFail getGeometry bs of
      Left (_, _, msg) ->
        let errMsg = "could not convert Geometry from db: " <> msg
        in returnError ConversionFailed field errMsg
      Right (_, _, geom) -> pure geom
  fromfield field Nothing = returnError UnexpectedNull field ""
