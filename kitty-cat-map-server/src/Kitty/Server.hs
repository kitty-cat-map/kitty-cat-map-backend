{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

module Kitty.Server
  ( module X
  , app
  , defaultMain
  ) where

import Control.Lens (view)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Server, serve)
import qualified Servant as Servant
import Servant.Utils.Enter ((:~>)(NT), enter)

import Kitty.Img (createImgDir)
import Kitty.Server.Handler as X
import Kitty.Server.Conf as X

app :: ServerConf -> Application
app config = serve (Proxy :: Proxy Api) apiServer
  where
    apiServer :: Server Api
    apiServer = enter naturalTrans serverRoot

    naturalTrans :: RIO ServerConf :~> Servant.Handler
    naturalTrans = NT transformation

    transformation :: forall a . RIO ServerConf a -> Servant.Handler a
    transformation = runRIO config

defaultMain :: MonadIO m => m ()
defaultMain = do
  conf <- mkServerConfEnv
  let port' = view port conf
  runReaderT createImgDir conf
  putStrLn $ "kitty-cat-map running on port " <> tshow port'
  liftIO . run port' $ app conf
