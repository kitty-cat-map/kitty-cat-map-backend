{-# LANGUAGE QuasiQuotes #-}

module Kitty.Db.Query where

import Control.Lens (view)
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.Base (liftBase)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple
       (Connection, FromRow, Only(Only), Query, ToRow, formatQuery, query,
        query_)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Kitty.Db.Conf (HasPool(pool))
import Kitty.Db.Geom (Geom(Geom), Lat, Lon)
import Kitty.Db.Model
       (ImgInfo'(ImgInfo, imageDate, imageFilename, imageGeom), ImgInfo,
        ImgInfoData, ImgInfoKey)

data QueryError = QueryError Text
  deriving (Eq, Read, Show)

instance Exception QueryError

newtype Offset = Offset { unOffset :: Int }
  deriving (Eq, FromJSON, Num, Ord, Read, Show, ToField, ToJSON)

newtype Limit = Limit { unLimit :: Int }
  deriving (Eq, FromJSON, Num, Ord, Read, Show, ToField, ToJSON)

runDb :: (MonadBaseControl IO m, MonadReader r m, HasPool r) => (Connection -> m a) -> m a
runDb f = do
  pool' <- view pool
  withResource pool' f

dbGetImages
  :: (MonadBaseControl IO m, MonadReader r m, HasPool r)
  => m [ImgInfo]
dbGetImages =
  runDb $
  quer_ "SELECT id, filename, date, ST_Y(geom), ST_X(geom) FROM image_info"

dbFindImages
  :: (MonadBaseControl IO m, MonadReader r m, HasPool r)
  => Lat -> Lat -> Lon -> Lon -> Offset -> Limit -> m [ImgInfo]
dbFindImages minLat maxLat minLon maxLon offset limit = do
  let q =
        [sql|
          SELECT id, filename, "date", ST_Y(geom), ST_X(geom)
          FROM image_info
          WHERE
            lat >= ? AND
            lat <= ? AND
            lon >= ? AND
            lon <= ?
          ORDER BY date DESC
          LIMIT ?
          OFFSET ?
          |]
  runDb $ quer q (minLat, maxLat, minLon, maxLon, offset, limit)

dbCreateImage
  :: (MonadBaseControl IO m, MonadReader r m, MonadThrow m, HasPool r)
  => ImgInfoData -> m ImgInfoKey
dbCreateImage ImgInfo {imageFilename, imageDate, imageGeom = Geom lat lon} = do
  let q =
        [sql|
          INSERT INTO image_info
            (filename, date, lat, lon, geom)
          VALUES
            (?, ?, ?, ?, ST_SetSRID(ST_POINT(?, ?), 4326))
          RETURNING id
          |]
  runDb $ querS q (imageFilename, imageDate, lat, lon, lon, lat)

quer
  :: (FromRow r, MonadBase IO m, ToRow q)
  => Query -> q -> Connection -> m [r]
quer query' substitution conn = liftBase $ query conn query' substitution

quer_
  :: (FromRow r, MonadBase IO m)
  => Query -> Connection -> m [r]
quer_ query' conn = liftBase $ query_ conn query'

querS
  :: (FromField r, MonadBase IO m, MonadThrow m, ToRow q)
  => Query -> q -> Connection -> m r
querS query' substitution conn = do
  res' <- quer query' substitution conn
  let fullQueryM = formatQuery conn query' substitution
  case res' of
    [] -> do
      formatted <- liftBase fullQueryM
      let msg =
            "received no results from q query that should return only a " <>
            "single result: " <> decodeUtf8 formatted
      throwIO $ QueryError msg
    [Only res] -> pure res
    _ -> do
      formatted <- liftBase fullQueryM
      let msg =
            "received multiple results from q query that should return only " <>
            "a single result: " <> decodeUtf8 formatted
      throwIO $ QueryError msg
