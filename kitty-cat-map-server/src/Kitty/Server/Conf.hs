
module Kitty.Server.Conf where

import Network.Wai.Handler.Warp (Port)

data ServerConf = ServerConf
  { serverConfPort :: Port
  }

-- -- | This 'Config' object is used to store environment about our application.
-- -- It is created on startup and passed to all the handlers.
-- data Conf = Conf
--   { {- confPool :: !ConnectionPool  -- ^ A pool of database connections.
--   , -} confPort :: !Port            -- ^ 'Port' to listen on.
--   }

-- -- | Number of simultaneous database connections to use in the
-- -- 'ConnectionPool'.
-- type DbPoolConnNum = Int

-- -- | Create a 'ConnectionPool' for database connections based on a
-- -- 'ConnectionString'.
-- makePoolFromUrl
--   :: DbPoolConnNum      -- ^ Number of database connections to use.
--   -> ConnectionString
--   -> IO ConnectionPool
-- makePoolFromUrl dbConnNum connectionString =
--   runStdoutLoggingT $ createPostgresqlPool connectionString dbConnNum

-- -- | Create a 'Config' based on environment variables, using defaults if the
-- -- environment variables don't exist.
-- createConfEnv :: IO Config
-- createConfigFromEnvVars = do
--   port <- readEnvVarDef "PORT" 8080
--   dbConnNum <- readEnvVarDef "DATABASE_CONNECTION_NUM" 10
--   dbConnectionString <-
--     lookupEnvDef
--       "DATABASE_URL"
--       "postgres://mydbuser:mydbpass@localhost:5432/mydb"
--   pool <- makePoolFromUrl dbConnNum dbConnectionString
--   pure Config {configPool = pool, configPort = port}

serverConfEnv :: IO ServerConf
serverConfEnv = undefined
