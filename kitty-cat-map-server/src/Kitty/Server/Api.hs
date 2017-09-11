
module Kitty.Server.Api where

import Control.Lens (view)
import Data.Attoparsec.Text (maybeResult, parse)
import Data.Attoparsec.Time (utcTime)
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
        dbCreateImage, dbFindImages, mkLat, mkLon)
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
  { filename :: FilePath
  , date :: UTCTime
  , geom :: Geom
  }

parseDate :: Text -> Maybe UTCTime
parseDate = maybeResult . parse utcTime 

instance FromMultipart PostImageForm where
  fromMultipart :: MultipartData -> Maybe PostImageForm
  fromMultipart multi = do
    tmpFile <- fdFilePath <$> listToMaybe (files multi)
    date <- parseDate =<< lookupInput "date" multi
    lat <- mkLat =<< readMay =<< lookupInput "lat" multi
    lon <- mkLon =<< readMay =<< lookupInput "lon" multi
    pure $ PostImageForm tmpFile date (Geom lat lon)

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
postImage PostImageForm{filename, date, geom} = do
  eitherImg <- copyImg filename
  case eitherImg of
    Left imgErr -> pureErrEnvelope imgErr
    Right imgPath -> do
      let imageInfo = ImageInfo () imgPath date geom
      imageId <- dbCreateImage imageInfo
      pureSuccEnvelope imageId

searchApi
  :: (HasPool r, MonadBaseControl IO m, MonadIO m, MonadReader r m)
  => ServerT SearchApi m
searchApi = getSearchImage

getSearchImage
  :: (HasPool r, MonadBaseControl IO m, MonadIO m, MonadReader r m)
  => m Int
getSearchImage = do
  images <- dbFindImages (-50) 50 0 100
  print images
  pure 3

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
