
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
        Lon(Lon), dbCreateImage, dbGetImages)
import Kitty.Server.Conf (ServerConf, mkServerConfEnv, port)

type Api = Image

type Image = "image" :> (PostImage :<|> GetImage)

type PostImage =
  MultipartForm  PostImageJson :>
  Throws Err :>
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

data PostImageJson = PostImageJson
  { geom :: Geom
  , filename :: FilePath
  }

instance FromMultipart PostImageJson where
  fromMultipart :: MultipartData -> Maybe PostImageJson
  fromMultipart multi = do
    lat <- fmap Lat . readMay =<< lookupInput "lat" multi
    lon <- fmap Lon . readMay =<< lookupInput "lon" multi
    tmpFile <- fdFilePath <$> listToMaybe (files multi)
    pure $ PostImageJson (Geom lat lon) tmpFile

serverRoot :: ServerT Api (RIO ServerConf)
serverRoot = postImage :<|> getImage

postImage
  :: (HasPool r, MonadBaseControl IO m, MonadReader r m, MonadThrow m)
  => PostImageJson -> m (Envelope '[ Err] ImageInfoKey)
postImage PostImageJson{geom, filename} = do
  let imageInfo = ImageInfo () "example_filename.jpg" geom
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

defaultMain :: IO ()
defaultMain = do
  conf <- mkServerConfEnv
  let port' = view port conf
  putStrLn $ "kitty-cat-map running on port " <> tshow port'
  run port' $ app conf
