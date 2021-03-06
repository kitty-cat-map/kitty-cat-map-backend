{-# LANGUAGE TemplateHaskell #-}

module Kitty.Img
  ( module Kitty.Img
  , module Kitty.Img.Conf
  ) where

import Control.Lens (_Wrapped, view)
import Crypto.Hash (Digest, SHA256)
import Crypto.Hash.Conduit (hashFile)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.ByteString (hGet)
import System.Directory
       (copyFile, createDirectoryIfMissing, doesFileExist)
import System.IO (IOMode(ReadMode), withFile)

import Kitty.Db (ImgFilename(ImgFilename))
import Kitty.Img.Conf
       (HasImgConf(imgConf), HasImgDir(imgDir), HasImgUrl(imgUrl),
        ImgUrl(ImgUrl), ImgConf(ImgConf, imgConfDir, imgConfUrl),
        ImgDir(ImgDir), ImgUrl(ImgUrl), mkImgConf)

data ImgErr
  = ImgErrHashErr
    -- ^ Error when trying to take the hash of the temporary file
  | ImgErrImgTypeErr
    -- ^ Error when reading the first few bytes of the temporary file to find
    -- its magic number
  | ImgErrNotImg
    -- ^ The input file did not have a correct magic number.
  | ImgErrCouldNotCopy
    -- ^ Error when trying to copy the temporary file to the images directory.
  deriving (Eq, Read, Show)

$(deriveJSON defaultOptions ''ImgErr)

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

copyImg :: (HasImgDir r, MonadCatch m, MonadIO m, MonadReader r m) => FilePath -> m (Either ImgErr ImgFilename)
copyImg tmpFile = runExceptT $ do
  imgsPath <- view $ imgDir . _Wrapped
  digest <-
    catch
      (showHash <$> hashFile tmpFile)
      (\(_ :: IOException) -> throwError ImgErrHashErr)
  ext <- imgExt <$> imgType tmpFile
  let newFilename = digest <.> ext
  let newFile = imgsPath </> newFilename
  eitherRes <- try $ do
    newFileExists <- liftIO $ doesFileExist newFile
    unless newFileExists . liftIO $ copyFile tmpFile newFile
  case eitherRes of
    Right () -> pure . ImgFilename $ pack newFilename
    Left (_ :: IOException) -> throwError ImgErrCouldNotCopy

createImgDir :: (HasImgDir r, MonadIO m, MonadReader r m) => m ()
createImgDir = do
  dir <- view $ imgDir . _Wrapped
  liftIO $ createDirectoryIfMissing True dir

imgFilenameToUrl :: (HasImgUrl r, MonadReader r m) => ImgFilename -> m Text
imgFilenameToUrl (ImgFilename filename) = do
  ImgUrl url <- view imgUrl
  pure $ url <> filename
