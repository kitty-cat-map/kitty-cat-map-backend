
module Kitty.Db.Query where

import Control.Lens (view)
import Control.Monad.Base (liftBase)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple
       (Connection, FromRow, Only(Only), Query, ToRow, formatQuery, query,
        query_)
import Database.PostgreSQL.Simple.FromField (FromField)

import Kitty.Db.Conf (HasPool(pool))
import Kitty.Db.Geom (Geom(Geom))
import Kitty.Db.Model
       (ImageInfo'(ImageInfo, imageDate, imageFileName, imageGeom), ImageInfo,
        ImageInfoData, ImageInfoKey)

data QueryError = QueryError Text
  deriving (Eq, Read, Show)

instance Exception QueryError

runDb :: (MonadBaseControl IO m, MonadReader r m, HasPool r) => (Connection -> m a) -> m a
runDb f = do
  pool' <- view pool
  withResource pool' f

dbGetImages :: (MonadBaseControl IO m, MonadReader r m, HasPool r) => m [ImageInfo]
dbGetImages = runDb $ quer_ "SELECT id, filename, date, ST_Y(geom), ST_X(geom) FROM image_info"

dbCreateImage
  :: (MonadBaseControl IO m, MonadReader r m, MonadThrow m, HasPool r)
  => ImageInfoData -> m ImageInfoKey
dbCreateImage ImageInfo {imageFileName, imageDate, imageGeom = Geom lat lon} = do
  runDb $
    querS
      "INSERT INTO image_info (filename, date, lat, lon, geom) values (?, ?, ?, ?, ST_SetSRID(ST_POINT(?, ?), 4326)) RETURNING id"
      (imageFileName, imageDate, lat, lon, lon, lat)

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
