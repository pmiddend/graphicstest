{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings     #-}
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

import           Control.Applicative    ((*>), (<$>), (<*), (<*>),Applicative)
import           Control.Category       ((>>>))
import           Control.Monad          ((>>=))
import           Data.Eq                ((==))
import Control.Monad.IO.Class(MonadIO,liftIO)
import           Data.Int               (Int)
import           Data.List              (filter, foldr, map)
import           Data.Map.Strict        (Map, empty, fromList, union)
import           Data.Maybe             (Maybe (Just, Nothing), mapMaybe)
import           Data.Text              (Text, pack)
import           Data.Traversable       (traverse)
import           Data.Tuple             (fst, snd)
import           Linear.V2              (V2 (..))
import           Prelude                (Char,($),Functor)
import           System.FilePath
import           Text.Parsec            (many1)
import           Text.Parsec.Char       (char, noneOf)
import           Text.Parsec.Combinator (eof, sepEndBy1)
import           Text.Parsec.Prim       (ParsecT, Stream, (<|>))
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
getDescFilesInDir dir = liftIO $ filter (takeExtension >>> (== ".txt")) <$> getFilesInDir dir

readMediaFiles :: forall a m.(Applicative m, MonadIO m) => ImageLoadFunction m a -> FilePath -> m (SurfaceMap a,AnimMap)
readMediaFiles loadImage fp = (,) <$> (foldr union empty <$> smaps) <*> (foldr union empty <$> amaps)
  where readSingle :: ImageDescFile -> m (SurfaceMap a,AnimMap)
        readSingle f = imageDescToSurface loadImage f >>= imageDescToMaps f
        maps :: m [(SurfaceMap a,AnimMap)]
        maps = getDescFilesInDir fp >>= traverse readSingle
        smaps :: m [SurfaceMap a]
        smaps = map fst <$> maps
        amaps :: m [AnimMap]
        amaps = map snd <$> maps

imageDescToSurface :: ImageLoadFunction m a -> ImageDescFile -> m a
imageDescToSurface loadImage x = loadImage (replaceExtension x ".png")

imageDescToMaps :: forall a m.(Functor m,Applicative m,MonadIO m) => ImageDescFile -> a -> m (SurfaceMap a,AnimMap)
imageDescToMaps f s = (,) <$> (toSurfaceMap s <$> rSurfaceData) <*> rAnimData
  where rImageData :: m [DataLine]
        rImageData = readImageData f
        rSurfaceData :: m ImageMap
        rSurfaceData = fromList <$> (mapMaybe (\x -> case x of
                                  DataLineImage i -> Just i
                                  _ -> Nothing) <$> rImageData)
        rAnimData :: m AnimMap
        rAnimData = fromList <$> (mapMaybe (\x -> case x of
                                  DataLineAnim i -> Just i
                                  _ -> Nothing) <$> rImageData)

toSurfaceMap :: a -> ImageMap -> SurfaceMap a
toSurfaceMap s = ((s,) <$>)

readImageData :: MonadIO m => FilePath -> m [DataLine]
readImageData fp = liftIO $ safeParseFromFile imageDataC fp

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
