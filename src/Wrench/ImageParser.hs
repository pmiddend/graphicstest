{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Wrench.ImageParser(DataLine(..),readImageDataFromFile,readImageDataFromText) where

import           ClassyPrelude        hiding (FilePath, (</>))
import           Data.Attoparsec.Text (Parser, char, many1, notInClass,
                                       parseOnly, satisfy, scientific, sepBy1)
import qualified Data.Text.IO         as TIO
import           Linear.V2            (V2 (..))
import           System.FilePath
import           Wrench.Animation
import           Wrench.AnimId
import           Wrench.ImageId
import           Wrench.Point
import           Wrench.Rectangle
import Data.Scientific(Scientific,toBoundedInteger,toRealFloat)

data DataLine = DataLineImage (ImageId,Rectangle)
              | DataLineAnim (AnimId,Animation) deriving(Eq,Show)

readImageDataFromFile :: MonadIO m => FilePath -> m [DataLine]
readImageDataFromFile f = liftM readImageDataFromText ((liftIO . TIO.readFile) f)

readImageDataFromText :: Text -> [DataLine]
readImageDataFromText t =
  case parseOnly imageDataC t of
    Left e -> error $ "Parse error: " <> e
    Right r -> r

imageDataC :: Parser [DataLine]
imageDataC = sepBy1 imageDataLineC (char '\n')

intOrError :: (Integral i,Bounded i) => Scientific -> i
intOrError s =
  case toBoundedInteger s of
    Nothing -> error $ "scientific \"" <> show s <>"\" is not an integer"
    Just s' -> s'

realFloatOrError :: RealFloat i => Scientific -> i
realFloatOrError s = toRealFloat s

scientificBoundedInt :: (Integral i,Bounded i) => Parser i
scientificBoundedInt = intOrError <$> scientific

scientificRealFloat :: RealFloat i => Parser i
scientificRealFloat = realFloatOrError <$> scientific

point :: Parser Point
point = V2 <$> scientificRealFloat <*> (char ',' *> scientificRealFloat)

rectangle :: Parser Rectangle
rectangle = rectangleFromPoints <$> point <*> (char ',' *> point)

imageDataLineC :: Parser DataLine
imageDataLineC = (char '>' *> (DataLineAnim <$> imageDataLineAnimC)) <|> (DataLineImage <$> imageDataLineImageC)

stringNotInClass :: String -> Parser Text
stringNotInClass s = pack <$> many1 (satisfy (notInClass s))

imageDataLineImageC :: Parser (ImageId,Rectangle)
imageDataLineImageC = (,) <$> stringNotInClass "=\n" <*> (char '=' *> rectangle)

imageDataLineAnimC :: Parser (AnimId,Animation)
imageDataLineAnimC = (,) <$> stringNotInClass "=\n" <*> (Animation <$> (char '=' *> scientificBoundedInt <* char '|') <*> sepBy1 (stringNotInClass  ",") (char ','))
