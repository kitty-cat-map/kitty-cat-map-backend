
module Kitty.Api where

import Data.Char (toLower)
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Handler, (:<|>)((:<|>)), ServerT, serve)
import Servant.Checked.Exceptions
       (Envelope, pureErrEnvelope, pureSuccEnvelope)

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
    maybe (fail "could not parse as Err") pure . readMaybe . unpack

serverRoot :: ServerT Api Handler
serverRoot = postImage

postImage :: Handler (Envelope '[Err] SearchResponse)
postImage = pure 1

app :: Application
app = serve (Proxy :: Proxy Api) serverRoot

port :: Int
port = 8201

defaultMain :: IO ()
defaultMain = run port app
