{-# LANGUAGE TemplateHaskell #-}

module Kitty.Server.Img where

import Control.Lens (Lens', _Wrapped, view)
import Control.Lens.TH (makeWrapped)
import Crypto.Hash (Digest, SHA256)
import Crypto.Hash.Conduit (hashFile)
import Data.ByteString (hGet)
import System.Directory
       (copyFile, createDirectoryIfMissing, doesFileExist)
import System.IO (IOMode(ReadMode), withFile)

newtype ImgDir = ImgDir { unImgDir :: FilePath }
  deriving (Eq, IsString, Read, Show)

$(makeWrapped ''ImgDir)

class HasImgDir s where
  imgDir :: Lens' s ImgDir

instance HasImgDir ImgDir where
  imgDir :: Lens' ImgDir ImgDir
  imgDir = id

data ImgErr
  = ImgErrHashErr
  | ImgErrImgTypeErr
  | ImgErrNotImg
  deriving (Eq, Read, Show)

instance Exception ImgErr

data ImgType = Jpeg | Png

reading :: MonadIO m => FilePath -> (ByteString -> r) -> m r
reading file test = liftIO $ withFile file ReadMode $ \h -> do
  bytes <- hGet h 32
  return (length bytes `seq` test bytes)

-- | Joint Photographic Experts Group (JPEG). Returns @Just "jpeg"@ if
-- file satisfies check.
testJpeg :: ByteString -> Bool
testJpeg bytes = take 4 (drop 6 bytes) `elem` ["JFIF", "Exif"]

-- | Portable Network Graphics (PNG). Returns @Just "png"@ if file
-- satisfies check against magic number @89 50 4e 47 0d 0a 1a 0a@.
testPng :: ByteString -> Bool
testPng = isPrefixOf "\137PNG\r\n\26\n"

imgTests :: [(ImgType, ByteString -> Bool)]
imgTests = [(Jpeg, testJpeg), (Png, testPng)]

getImgType :: ByteString -> Maybe ImgType
getImgType bs = fst <$> find (\(_, test) -> test bs) imgTests

imgType :: (MonadCatch m, MonadError ImgErr m, MonadIO m) => FilePath -> m ImgType
imgType file = do
  maybeImgType <-
    handle
      (\(_ :: IOException) -> throwError ImgErrImgTypeErr)
      (reading file getImgType)
  maybe (throwError ImgErrNotImg) pure maybeImgType

imgExt :: ImgType -> String
imgExt Jpeg = "jpg"
imgExt Png = "png"

showHash :: Digest SHA256 -> String
showHash = show

copyImg :: (HasImgDir r, MonadCatch m, MonadIO m, MonadReader r m) => FilePath -> m (Either ImgErr FilePath)
copyImg tmpFile = runExceptT $ do
  imgsPath <- view $ imgDir . _Wrapped
  digest <-
    catch
      (showHash <$> hashFile tmpFile)
      (\(_ :: IOException) -> throwError ImgErrHashErr)
  ext <- imgExt <$> imgType tmpFile
  let newFile = imgsPath </> digest <.> ext
  newFileExists <- liftIO $ doesFileExist newFile
  unless newFileExists . liftIO $ copyFile tmpFile newFile
  pure newFile

createImgDir :: (HasImgDir r, MonadIO m, MonadReader r m) => m ()
createImgDir = do
  dir <- view $ imgDir . _Wrapped
  liftIO $ createDirectoryIfMissing True dir
