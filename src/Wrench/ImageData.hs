{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Wrench.ImageData(
  ImageId,
  ImageMap,
  AnimMap,
  readMediaFiles,
  animFrameSwitch,
  animFrames,
  SurfaceMap,
  findSurfaceUnsafe,
  SurfaceData
  ) where

import           Control.Category          ((>>>))
import qualified Data.Map.Strict           as M
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

type ImageDescFile = FilePath

type SurfaceData a = (a,Rectangle)

type SurfaceMap a = Map ImageId (SurfaceData a)

type ImageLoadFunction m a = FilePath -> m a

findSurfaceUnsafe :: SurfaceMap a -> ImageId -> SurfaceData a
findSurfaceUnsafe sm im = fromMaybe (error $ "Cannot find image \"" <> T.unpack im <> "\" in " <> (show (keys sm))) (im `M.lookup` sm)

-- Holt alle "Descriptorfiles" (also die mit .txt enden) aus dem Directory
getDescFilesInDir :: MonadIO m => FilePath -> m [ImageDescFile]
getDescFilesInDir dir = liftIO $ filter (takeExtension >>> (== ".txt")) <$> getFilesInDir dir

readMediaFiles :: forall a m.(Applicative m, MonadIO m) => ImageLoadFunction m a -> FilePath -> m (SurfaceMap a,AnimMap)
readMediaFiles loadImage fp = (,) <$> (foldr M.union M.empty <$> smaps) <*> (foldr M.union M.empty <$> amaps)
  where readSingle :: ImageDescFile -> m (SurfaceMap a,AnimMap)
        readSingle f = imageDescToSurface loadImage f >>= imageDescToMaps f
        maps :: m [(SurfaceMap a,AnimMap)]
        maps = getDescFilesInDir fp >>= traverse readSingle
        smaps :: m [SurfaceMap a]
        smaps = map fst <$> maps
        amaps :: m [AnimMap]
        amaps = map snd <$> maps

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
