
module Kitty.Server.ImgFileServer where

import Control.Lens (view)
import Network.Wai
       (Application, Request, Response, ResponseReceived)
import Network.Wai.Application.Static
       (defaultWebAppSettings, staticApp)
import Servant (Context, HasServer(route), ServerT, runHandler)
import qualified Servant as Servant
import Servant.Server.Internal
       (Delayed, Router'(RawRouter), RouteResult(Fail, FailFatal, Route), responseServantErr,
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
    -> Delayed env (Servant.Handler Application)
    -> Router' env (Request -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived)
  route Proxy _ rawApplication = RawRouter go
    where
      go
        :: env
        -> Request
        -> (RouteResult Response -> IO ResponseReceived)
        -> IO ResponseReceived
      go env request respond =
        runResourceT $ do
          routeRes <- runDelayed rawApplication env request
          liftIO $
            case routeRes of
              (Fail e) -> respond $ Fail e
              (FailFatal e) -> respond $ FailFatal e
              (Route handlerApp) -> do
                eitherApp <- runHandler handlerApp
                case eitherApp of
                  Left err -> respond . Route $ responseServantErr err
                  Right app -> app request (respond . Route)

rawImgServer :: (HasImgDir r, MonadReader r m) => m Application
rawImgServer = do
  ImgDir dir <- view imgDir
  pure $ staticApp $ defaultWebAppSettings $ addTrailingPathSeparator dir
