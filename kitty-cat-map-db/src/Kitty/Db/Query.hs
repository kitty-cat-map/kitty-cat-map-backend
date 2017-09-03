
module Kitty.Db.Query where

import Control.Lens (view)
import Control.Monad.Base (liftBase)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (Connection, FromRow, Query, ToRow, query, query_)

import Kitty.Db.Conf (HasPool(pool))
import Kitty.Db.Model (ImageInfo)

-- hello :: IO Int
-- hello = do
--   conn <- connectPostgreSQL ""
--   [Only i] <- query_ conn "select 2 + 2"
--   pure i

runDb :: (MonadBaseControl IO m, MonadReader r m, HasPool r) => (Connection -> m a) -> m a
runDb f = do
  pool' <- view pool
  withResource pool' f

dbGetImage :: (MonadBaseControl IO m, MonadReader r m, HasPool r) => m [ImageInfo]
dbGetImage = runDb $ quer_ "SELECT * from image_info"

quer
  :: (FromRow r, MonadBase IO m, ToRow q)
  => Query -> q -> Connection -> m [r]
quer query' substitution conn = liftBase $ query conn query' substitution

quer_
  :: (FromRow r, MonadBase IO m)
  => Query -> Connection -> m [r]
quer_ query' conn = liftBase $ query_ conn query'
