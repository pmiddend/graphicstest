{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Wrench.ImageParser(DataLine(..),readImageDataFromFile,readImageDataFromText) where

import           Linear.V2                 (V2 (..))
import           Text.Parsec               (many1)
import           Text.Parsec.Char          (char, noneOf)
import           Text.Parsec.Combinator    (eof, sepEndBy1)
import           ClassyPrelude hiding(FilePath,(</>))
import System.FilePath
import           Text.Parsec.Prim          (ParsecT, Stream)
import           Wrench.Parsec
import           Wrench.Point
import           Wrench.Rectangle
import Wrench.ImageId
import Wrench.AnimId
import Wrench.Animation

data DataLine = DataLineImage (ImageId,Rectangle)
              | DataLineAnim (AnimId,Animation) deriving(Eq,Show)

readImageDataFromFile :: MonadIO m => FilePath -> m [DataLine]
readImageDataFromFile fp = liftIO $ safeParseFromFile imageDataC fp

readImageDataFromText :: Text -> [DataLine]
readImageDataFromText = safeParseFromText imageDataC

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
