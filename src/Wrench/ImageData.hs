{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import           Control.Applicative    ((*>), (<$>), (<*), (<*>))
import           Control.Category       ((>>>))
import           Control.Monad          ((>>=))
import           Data.Eq                ((==))
import           Data.Int               (Int)
import           Data.List              (filter, foldr, map)
import           Data.Map.Strict        (Map, empty, fromList, union)
import           Data.Maybe             (Maybe (Just, Nothing), mapMaybe)
import           Data.Text              (Text, pack)
import           Data.Traversable       (traverse)
import           Data.Tuple             (fst, snd)
import           Graphics.UI.SDL.Image  (loadTexture)
import qualified Graphics.UI.SDL.Types  as SDLT
import           Linear.V2              (V2 (..))
import           Prelude                (Char)
import           System.FilePath
import           System.IO              (IO)
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

type SurfaceData = (SDLT.Texture,Rectangle)

type SurfaceMap = Map ImageId SurfaceData

data Animation = Animation {
  animFrameSwitch :: Int,
  animFrames      :: [ImageId]
  }

type AnimId = Text

type AnimMap = Map AnimId Animation

-- Holt alle "Descriptorfiles" (also die mit .txt enden) aus dem Directory
getDescFilesInDir :: FilePath -> IO [ImageDescFile]
getDescFilesInDir dir = filter (takeExtension >>> (== ".txt")) <$> getFilesInDir dir

readMediaFiles :: SDLT.Renderer -> FilePath -> IO (SurfaceMap,AnimMap)
readMediaFiles r fp = (,) <$> (foldr union empty <$> smaps) <*> (foldr union empty <$> amaps)
  where readSingle :: ImageDescFile -> IO (SurfaceMap,AnimMap)
        readSingle f = imageDescToSurface r f >>= imageDescToMaps f
        maps :: IO [(SurfaceMap,AnimMap)]
        maps = getDescFilesInDir fp >>= traverse readSingle
        smaps :: IO [SurfaceMap]
        smaps = map fst <$> maps
        amaps :: IO [AnimMap]
        amaps = map snd <$> maps

loadImage :: SDLT.Renderer -> FilePath -> IO SDLT.Texture
loadImage = loadTexture

imageDescToSurface :: SDLT.Renderer -> ImageDescFile -> IO SDLT.Texture
imageDescToSurface r x = loadImage r (replaceExtension x ".png")

imageDescToMaps :: ImageDescFile -> SDLT.Texture -> IO (SurfaceMap,AnimMap)
imageDescToMaps f s = (,) <$> (toSurfaceMap s <$> rSurfaceData) <*> rAnimData
  where rImageData :: IO [DataLine]
        rImageData = readImageData f
        rSurfaceData :: IO ImageMap
        rSurfaceData = fromList <$> (mapMaybe (\x -> case x of
                                  DataLineImage i -> Just i
                                  _ -> Nothing) <$> rImageData)
        rAnimData :: IO AnimMap
        rAnimData = fromList <$> (mapMaybe (\x -> case x of
                                  DataLineAnim i -> Just i
                                  _ -> Nothing) <$> rImageData)

toSurfaceMap :: SDLT.Texture -> ImageMap -> SurfaceMap
toSurfaceMap s = ((s,) <$>)

readImageData :: FilePath -> IO [DataLine]
readImageData = safeParseFromFile imageDataC

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
