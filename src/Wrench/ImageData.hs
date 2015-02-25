{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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
  SurfaceData
  ) where

import           Control.Category          ((>>>))
import qualified Data.Map.Strict           as M
import           Linear.V2                 (V2 (..))
import           Text.Parsec               (many1)
import           Text.Parsec.Char          (char, noneOf)
import           Text.Parsec.Combinator    (eof, sepEndBy1)
import           ClassyPrelude
import           Filesystem.Path.CurrentOS
import           Text.Parsec.Prim          (ParsecT, Stream)
import           Wrench.Filesystem
import           Wrench.Parsec
import           Wrench.Point
import           Wrench.Rectangle

data DataLine = DataLineImage (ImageId,Rectangle) | DataLineAnim (AnimId,Animation)

type ImageId = Text

type ImageMap = Map ImageId Rectangle

type ImageDescFile = FilePath

type SurfaceData a = (a,Rectangle)

type SurfaceMap a = Map ImageId (SurfaceData a)

data Animation = Animation {
  animFrameSwitch :: Int,
  animFrames      :: [ImageId]
  }

type AnimId = Text

type AnimMap = Map AnimId Animation

type ImageLoadFunction m a = FilePath -> m a

-- Holt alle "Descriptorfiles" (also die mit .txt enden) aus dem Directory
getDescFilesInDir :: MonadIO m => FilePath -> m [ImageDescFile]
getDescFilesInDir dir = liftIO $ filter (extension >>> (== Just "txt")) <$> getFilesInDir dir

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
        rImageData = readImageData f
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

readImageData :: MonadIO m => FilePath -> m [DataLine]
readImageData fp = liftIO $ safeParseFromFile imageDataC (fpToString fp)

imageDataC :: Stream s m Char => ParsecT s u m [DataLine]
imageDataC = sepEndBy1 imageDataLineC (char '\n') <* eof

point :: Stream s m Char => ParsecT s u m Point
point = V2 <$> floatType <*> (char ',' *> floatType)

rectangle :: Stream s m Char => ParsecT s u m Rectangle
rectangle = rectangleFromPoints <$> point <*> (char ',' *> point)

imageDataLineC :: Stream s m Char => ParsecT s u m DataLine
imageDataLineC = (char '>' *> (DataLineAnim <$> imageDataLineAnimC)) <|> (DataLineImage <$> imageDataLineImageC)

imageDataLineImageC :: Stream s m Char => ParsecT s u m (ImageId,Rectangle)
imageDataLineImageC = (,) <$> (pack <$> many1 (noneOf "=\n")) <*> (char '=' *> rectangle)

imageDataLineAnimC :: Stream s m Char => ParsecT s u m (AnimId,Animation)
imageDataLineAnimC = (,) <$> (pack <$> many1 (noneOf "=\n")) <*> (Animation <$> (char '=' *> int <* char '|') <*> sepEndBy1 (pack <$> many1 (noneOf ",\n")) (char ','))
