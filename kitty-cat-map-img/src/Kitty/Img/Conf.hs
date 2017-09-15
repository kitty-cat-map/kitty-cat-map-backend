{-# LANGUAGE TemplateHaskell #-}

module Kitty.Img.Conf where

import Control.Lens (Lens', lens)
import Control.Lens.TH (makeWrapped)

newtype ImgDir = ImgDir { unImgDir :: FilePath }
  deriving (Eq, IsString, Read, Show)

$(makeWrapped ''ImgDir)

class HasImgDir s where
  imgDir :: Lens' s ImgDir

instance HasImgDir ImgDir where
  imgDir :: Lens' ImgDir ImgDir
  imgDir = id

newtype ImgUrl = ImgUrl { unImgUrl :: Text }
  deriving (Eq, IsString, Read, Show)

$(makeWrapped ''ImgUrl)

class HasImgUrl s where
  imgUrl :: Lens' s ImgUrl

instance HasImgUrl ImgUrl where
  imgUrl :: Lens' ImgUrl ImgUrl
  imgUrl = id


data ImgConf = ImgConf
  { imgConfDir :: ImgDir
  , imgConfUrl :: ImgUrl
  }

class HasImgConf s where
  imgConf :: Lens' s ImgConf

instance HasImgConf ImgConf where
  imgConf :: Lens' ImgConf ImgConf
  imgConf = id

instance HasImgDir ImgConf where
  imgDir :: Lens' ImgConf ImgDir
  imgDir = lens imgConfDir (\s a -> s { imgConfDir = a })

instance HasImgUrl ImgConf where
  imgUrl :: Lens' ImgConf ImgUrl
  imgUrl = lens imgConfUrl (\s a -> s { imgConfUrl = a })

mkImgConf
  :: MonadIO m
  => ImgDir -> ImgUrl -> m ImgConf
mkImgConf imgDir' imgUrl' =
  pure $ ImgConf {imgConfDir = imgDir', imgConfUrl = imgUrl'}
