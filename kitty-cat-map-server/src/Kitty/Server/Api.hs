
module Kitty.Server.Api where

import Prelude hiding (Handler)

import Data.Aeson
       (FromJSON, ToJSON, Value, parseJSON, toJSON, withText)
import Data.Aeson.Types (Parser)
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
       (Handler(Handler), JSON, Post, Server, ServerT, (:>), (:<|>)((:<|>)), serve)
import Servant.Checked.Exceptions
       (Envelope, Throws, pureErrEnvelope, pureSuccEnvelope)
import Servant.Utils.Enter ((:~>)(NT), enter)

import Kitty.Server.Conf (ServerConf(serverConfPort), serverConfEnv)

type Api = Image

type Image = "image" :> PostImage

type PostImage =
  Throws Err :>
  Post '[JSON] Int

data Err = Err deriving (Eq, Read, Show)

instance ToJSON Err where
  toJSON :: Err -> Value
  toJSON = toJSON . show

instance FromJSON Err where
  parseJSON :: Value -> Parser Err
  parseJSON = withText "Err" $
    maybe (fail "could not parse as Err") pure . readMay . unpack

serverRoot :: ServerT Api (RIO ServerConf)
serverRoot = postImage

postImage :: RIO ServerConf (Envelope '[Err] Int)
postImage = pureSuccEnvelope 1


-- | Create a WAI 'Application' capable of running with Warp.
app :: ServerConf -> Application
app config = serve (Proxy :: Proxy Api) apiServer
  where
    apiServer :: Server Api
    apiServer = enter naturalTrans serverRoot

    naturalTrans :: RIO ServerConf :~> Handler
    naturalTrans = NT transformation

    -- This represents a natural transformation from 'MyApiM' to 'Handler'.
    -- This consists of unwrapping the 'MyApiM', running the
    -- @'ReaderT' 'ServerConf'@, and wrapping the resulting value back up in a
    -- 'Handler'.
    transformation :: forall a . RIO ServerConf a -> Handler a
    transformation = Handler . lift . flip runRIO config

port :: Int
port = 8201

defaultMain :: IO ()
defaultMain = do
  conf <- serverConfEnv
  run (serverConfPort conf) $ app conf
