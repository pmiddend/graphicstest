{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Wrench.ImageData(
  ImageId,
  ImageMap,
  AnimMap,
  animFrameSwitch,
  animFrames,
  SurfaceMap,
  findSurfaceUnsafe,
  SurfaceData,
  ImageDescFile,
  imageDescToSurface,
  ImageLoadFunction,
  getDescFilesInDir,
  imageDescToMaps
  ) where

import           ClassyPrelude hiding(FilePath,(</>))
import qualified Data.Text as T
import System.FilePath
import           Wrench.Filesystem
import           Wrench.Rectangle
import Wrench.ImageParser
import Wrench.ImageId
import Wrench.AnimMap
import Wrench.Animation
import Wrench.ImageMap
import qualified Data.Map.Strict           as M

type ImageDescFile = FilePath

type SurfaceData a = (a,Rectangle)

type SurfaceMap a = Map ImageId (SurfaceData a)

type ImageLoadFunction m a = FilePath -> m a

findSurfaceUnsafe :: SurfaceMap a -> ImageId -> SurfaceData a
findSurfaceUnsafe sm im = fromMaybe (error $ "Cannot find image \"" <> T.unpack im <> "\" in " <> (show (keys sm))) (im `M.lookup` sm)

-- Holt alle "Descriptorfiles" (also die mit .txt enden) aus dem Directory
getDescFilesInDir :: (Functor m,MonadIO m) => FilePath -> m [ImageDescFile]
getDescFilesInDir dir = getFilesWithExtInDir dir ".txt"

imageDescToSurface :: ImageLoadFunction m a -> ImageDescFile -> m a
imageDescToSurface loadImage x = loadImage (replaceExtension x "png")

imageDescToMaps :: forall a m.(Functor m,Applicative m,MonadIO m) => ImageDescFile -> a -> m (SurfaceMap a,AnimMap)
imageDescToMaps f s = (,) <$> (toSurfaceMap s <$> rSurfaceData) <*> rAnimData
  where rImageData :: m [DataLine]
        rImageData = readImageDataFromFile f
        rSurfaceData :: m ImageMap
        rSurfaceData = M.fromList <$> (mapMaybe (\x -> case x of
                                  DataLineImage i -> Just i
                                  _ -> Nothing) <$> rImageData)
        rAnimData :: m AnimMap
        rAnimData = M.fromList <$> (mapMaybe (\x -> case x of
                                  DataLineAnim i -> Just i
                                  _ -> Nothing) <$> rImageData)

toSurfaceMap :: a -> ImageMap -> SurfaceMap a
toSurfaceMap s = ((s,) <$>)
