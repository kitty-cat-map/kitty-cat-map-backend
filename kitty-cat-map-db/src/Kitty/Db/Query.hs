
module Kitty.Db.Query where

import Control.Lens (view)
import Control.Monad.Base (liftBase)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple
       (Connection, FromRow, Query, ToRow, formatQuery, query, query_)

import Kitty.Db.Conf (HasPool(pool))
import Kitty.Db.Model
       (ImageInfo'(ImageInfo, imageFileName, imageGeom), ImageInfo,
        ImageInfoData, ImageInfoKey)

data QueryError = QueryError Text
  deriving (Eq, Read, Show)

instance Exception QueryError

-- hello :: IO Int
-- hello = do
--   conn <- connectPostgreSQL ""
--   [Only i] <- query_ conn "select 2 + 2"
--   pure i

runDb :: (MonadBaseControl IO m, MonadReader r m, HasPool r) => (Connection -> m a) -> m a
runDb f = do
  pool' <- view pool
  withResource pool' f

dbGetImages :: (MonadBaseControl IO m, MonadReader r m, HasPool r) => m [ImageInfo]
dbGetImages = runDb $ quer_ "SELECT * FROM image_info"

dbCreateImage
  :: (MonadBaseControl IO m, MonadReader r m, MonadThrow m, HasPool r)
  => ImageInfoData -> m ImageInfoKey
dbCreateImage ImageInfo {imageFileName, imageGeom} = do
  runDb $
    querS
      "INSERT INTO image_info (filename, geom) values (?, ?) RETURNING id"
      (imageFileName, imageGeom)

quer
  :: (FromRow r, MonadBase IO m, ToRow q)
  => Query -> q -> Connection -> m [r]
quer query' substitution conn = liftBase $ query conn query' substitution

quer_
  :: (FromRow r, MonadBase IO m)
  => Query -> Connection -> m [r]
quer_ query' conn = liftBase $ query_ conn query'

querS
  :: (FromRow r, MonadBase IO m, MonadThrow m, ToRow q)
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
    [res] -> pure res
    _ -> do
      formatted <- liftBase fullQueryM
      let msg =
            "received multiple results from q query that should return only " <>
            "a single result: " <> decodeUtf8 formatted
      throwIO $ QueryError msg
