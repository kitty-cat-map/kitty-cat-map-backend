
module Kitty.Server.Image where

import Control.Lens (Lens')

newtype ImgDir = ImgDir { unImgDir :: FilePath }
  deriving (Eq, IsString, Read, Show)

class HasImgDir s where
  imgDir :: Lens' s ImgDir

instance HasImgDir ImgDir where
  imgDir :: Lens' ImgDir ImgDir
  imgDir = id

-- reading :: FilePath -> (ByteString -> r) -> m r
-- reading file test = withFile file ReadMode $ \h -> do
--   bytes <- hGet h 32
--   return (length bytes `seq` test bytes)

-- -- | Joint Photographic Experts Group (JPEG). Returns @Just "jpeg"@ if
-- -- file satisfies check.
-- testJpeg :: ByteString -> Maybe String
-- testJpeg bytes = [ "jpeg"
--                  | take 4 (drop 6 bytes) `elem` ["JFIF", "Exif"]
--                  ]

-- -- | Portable Network Graphics (PNG). Returns @Just "png"@ if file
-- -- satisfies check against magic number @89 50 4e 47 0d 0a 1a 0a@.
-- testPng :: ByteString -> Maybe String
-- testPng bytes = [ "png"
--                 | isPrefixOf "\137PNG\r\n\26\n" bytes
-- ]

-- copyImg :: FilePath -> m ()
-- copyImg tmpFile = do
--   imgsPath <- view imgDir
--   digest <- hashFile tmpFile
--   let newFilePath = imgsPath </> tmpFileBase
