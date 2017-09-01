{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kitty.Db.Geom
  ( module Database.Postgis
  ) where

import Data.Binary.Builder (fromLazyByteString)
import Data.Binary.Get (runGetOrFail)
import Database.PostgreSQL.Simple.FromField
       (Conversion, Field, FromField,
        ResultError(ConversionFailed, UnexpectedNull), fromField,
        returnError)
import Database.PostgreSQL.Simple.ToField
       (Action(Plain), ToField, toField)
import Database.Postgis (Geometry, writeGeometry)
import Database.Postgis.Serialize (getGeometry)

instance ToField Geometry where
  toField  =  Plain . fromLazyByteString . writeGeometry

instance FromField Geometry where
  fromField :: Field -> Maybe ByteString -> Conversion Geometry
  fromField field (Just bs) =
    case runGetOrFail getGeometry (fromStrict bs) of
      Left (_, _, msg) ->
        let errMsg = "could not convert Geometry from db: " <> msg
        in returnError ConversionFailed field errMsg
      Right (_, _, geom) -> pure geom
  fromField field Nothing = returnError UnexpectedNull field ""
