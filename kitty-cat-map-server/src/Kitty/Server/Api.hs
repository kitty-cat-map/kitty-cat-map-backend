
module Kitty.Server.Api where

import Control.Lens (view)
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
       (Get, JSON, Post, Server, ServerT, (:>), (:<|>)((:<|>)), serve)
import qualified Servant as Servant
import Servant.Checked.Exceptions
       (Envelope, Throws, pureErrEnvelope, pureSuccEnvelope)
import Servant.Multipart
       (FileData(fdFilePath), FromMultipart(fromMultipart),
        MultipartData(files), MultipartForm, lookupInput)
import Servant.Utils.Enter ((:~>)(NT), enter)

import Kitty.Db
       (Geom(Geom), HasPool, ImageInfo'(ImageInfo), ImageInfoKey,
        dbCreateImage, mkLat, mkLon)
import Kitty.Server.Conf (ServerConf, mkServerConfEnv, port)
import Kitty.Server.Img (HasImgDir, ImgErr, copyImg, createImgDir)

type Api = ImgApi :<|> SearchApi

type ImgApi = "image" :> PostImage

type PostImage =
  MultipartForm PostImageForm :>
  Throws ImgErr :>
  Post '[JSON] ImageInfoKey

type SearchApi = "search" :> GetSearchImage

type GetSearchImage =
  "image" :>
  Get '[JSON] Int

data PostImageForm = PostImageForm
  { geom :: Geom
  , filename :: FilePath
  }

instance FromMultipart PostImageForm where
  fromMultipart :: MultipartData -> Maybe PostImageForm
  fromMultipart multi = do
    lat <- mkLat =<< readMay =<< lookupInput "lat" multi
    lon <- mkLon =<< readMay =<< lookupInput "lon" multi
    tmpFile <- fdFilePath <$> listToMaybe (files multi)
    pure $ PostImageForm (Geom lat lon) tmpFile

imgApi
  :: ( HasImgDir r
     , HasPool r
     , MonadBaseControl IO m
     , MonadCatch m
     , MonadIO m
     , MonadReader r m
     , MonadThrow m
     )
  => ServerT ImgApi m
imgApi = postImage

postImage
  :: ( HasImgDir r
     , HasPool r
     , MonadBaseControl IO m
     , MonadCatch m
     , MonadIO m
     , MonadReader r m
     , MonadThrow m
     )
  => PostImageForm -> m (Envelope '[ImgErr] ImageInfoKey)
postImage PostImageForm{geom, filename} = do
  eitherImg <- copyImg filename
  case eitherImg of
    Left imgErr -> pureErrEnvelope imgErr
    Right imgPath -> do
      let imageInfo = ImageInfo () imgPath geom
      imageId <- dbCreateImage imageInfo
      pureSuccEnvelope imageId

searchApi :: MonadBaseControl IO m => ServerT SearchApi m
searchApi = getSearchImage

getSearchImage :: MonadBaseControl IO m => m Int
getSearchImage = pure 3

serverRoot
  :: ( HasImgDir r
     , HasPool r
     , MonadBaseControl IO m
     , MonadCatch m
     , MonadIO m
     , MonadReader r m
     , MonadThrow m
     )
  => ServerT Api m
serverRoot = imgApi :<|> searchApi

-- | Create a WAI 'Application' capable of running with Warp.
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
