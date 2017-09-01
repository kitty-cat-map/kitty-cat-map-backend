
module Kitty.Api where

import Prelude hiding (Handler)

import Data.Aeson
       (FromJSON, ToJSON, Value, parseJSON, toJSON, withText)
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
       (Handler, JSON, Post, ServerT, (:>), (:<|>)((:<|>)), serve)
import Servant.Checked.Exceptions
       (Envelope, Throws, pureErrEnvelope, pureSuccEnvelope)

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

serverRoot :: ServerT Api (RIO Config)
serverRoot = postImage

postImage :: RIO Config (Envelope '[Err] Int)
postImage = pureSuccEnvelope 1

app :: Application
app = serve (Proxy :: Proxy Api) serverRoot

-- | Create a WAI 'Application' capable of running with Warp.
app :: Config -> Application
app config = serve (Proxy :: Proxy API) apiServer
  where
    apiServer :: Server API
    apiServer = enter naturalTrans serverRoot

    naturalTrans :: RIO Config :~> Handler
    naturalTrans = NT transformation

    -- This represents a natural transformation from 'MyApiM' to 'Handler'.
    -- This consists of unwrapping the 'MyApiM', running the
    -- @'ReaderT' 'Config'@, and wrapping the resulting value back up in a
    -- 'Handler'.
    transformation :: forall a . RIO Config a -> Handler a
    transformation = Handler . flip runReaderT config . unMyApiM

port :: Int
port = 8201

defaultMain :: IO ()
defaultMain = run port app
