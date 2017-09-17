
module Kitty.Server.ImgFileServer where

import Control.Lens (view)
import Network.Wai
       (Application, Request, Response, ResponseReceived)
import Network.Wai.Application.Static
       (defaultWebAppSettings, staticApp)
import Servant (Context, HasServer(route), ServerT, runHandler)
import qualified Servant as Servant
import Servant.Server.Internal
       (Delayed, Router'(RawRouter), RouteResult(Fail, FailFatal, Route),
        runDelayed)
import System.FilePath (addTrailingPathSeparator)

import Kitty.Img (HasImgDir(imgDir), ImgDir(ImgDir))

data RawImg deriving Typeable

instance HasServer RawImg context where
  type ServerT RawImg m = m Application
  route
    :: forall env.
       Proxy RawImg
    -> Context context
    -- -> Delayed env (ServerT RawImg Servant.Handler)
    -> Delayed env (Servant.Handler Application)
    -- -> Router env
    -- -> Router' env RoutingApplication
    -> Router' env (Request -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived)
  route Proxy _ rawApplication =
    RawRouter blahblah
    -- $ \env request respond ->
        -- note: a Raw application doesn't register any cleanup
        -- but for the sake of consistency, we nonetheless run
        -- the cleanup once its done
       -- do
        -- r <- runDelayed {- rawApplication -} undefined env request
        -- runAction rawApplication env request f g
        -- liftIO $ go r request respond
    where
      go :: RouteResult a -> Request -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived
      go r request respond =
        case r of
          Route app -> untag {- app -} undefined request (respond . Route)
          Fail a -> respond $ Fail a
          FailFatal e -> respond $ FailFatal e

      blahblah :: env -> Request -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived
      blahblah env request respond = do
        -- runAction rawApplication env request f g
        lala <- runResourceT $ runDelayed rawApplication env request
        papa request respond lala

      papa :: Request -> (RouteResult Response -> IO ResponseReceived) -> RouteResult (Servant.Handler Application) -> IO ResponseReceived
      papa _ respond (Fail e) = respond $ Fail e
      papa _ respond (FailFatal e) = respond $ FailFatal e
      papa request respond (Route handlerApp) = baba request respond handlerApp

      baba :: Request -> (RouteResult Response -> IO ResponseReceived) -> Servant.Handler Application -> IO ResponseReceived
      baba request respond handlerApp = do
        eitherApp <- runHandler handlerApp
        case eitherApp of
          Left err -> respond $ Fail err
          Right app -> app request (respond . Route)


rawImgServer :: (HasImgDir r, MonadReader r m) => m Application
rawImgServer = do
  ImgDir dir <- view imgDir
  pure $ staticApp $ defaultWebAppSettings $ addTrailingPathSeparator dir
