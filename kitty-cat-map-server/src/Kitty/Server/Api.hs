
module Kitty.Server.Api where

import Control.Lens (view)
import Data.Aeson
       (FromJSON, ToJSON, Value, parseJSON, toJSON, withText)
import Data.Aeson.Types (Parser)
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
       (Get, JSON, Post, ReqBody, Server, ServerT, (:>), (:<|>)((:<|>)),
        serve)
import qualified Servant as Servant
import Servant.Checked.Exceptions
       (Envelope, Throws, pureErrEnvelope, pureSuccEnvelope)
import Servant.Multipart
       (FileData(fdFilePath), FromMultipart(fromMultipart),
        MultipartData(files), MultipartForm, lookupInput)
import Servant.Utils.Enter ((:~>)(NT), enter)

import Kitty.Db
       (Geom(Geom), HasPool, ImageInfo'(ImageInfo), ImageInfoKey, Lat(Lat),
        Lon(Lon), dbCreateImage, dbGetImages, mkLat, mkLon)
import Kitty.Server.Conf (ServerConf, mkServerConfEnv, port)
import Kitty.Server.Img (HasImgDir, ImgErr, copyImg, createImgDir)

type Api = Image

type Image = "image" :> (PostImage :<|> GetImage)

type PostImage =
  MultipartForm PostImageForm :>
  Throws ImgErr :>
  Post '[JSON] ImageInfoKey

type GetImage =
  Throws Err :>
  Get '[JSON] Int

data Err = Err deriving (Eq, Read, Show)

instance ToJSON Err where
  toJSON :: Err -> Value
  toJSON = toJSON . show

instance FromJSON Err where
  parseJSON :: Value -> Parser Err
  parseJSON = withText "Err" $
    maybe (fail "could not parse as Err") pure . readMay . unpack

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

serverRoot :: ServerT Api (RIO ServerConf)
serverRoot = postImage :<|> getImage

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

getImage :: RIO ServerConf (Envelope '[Err] Int)
getImage = do
  images <- dbGetImages
  print images
  pureErrEnvelope Err

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
