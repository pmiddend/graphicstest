{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Wrench.ImageParser(DataLine(..),readImageDataFromFile,readImageDataFromText) where

import           ClassyPrelude        hiding (FilePath, (</>))
import           Data.Attoparsec.Text (Parser, char, many1, notInClass,
                                       parseOnly, satisfy, scientific, sepBy1)
import           Data.Scientific      (Scientific, toBoundedInteger)
import qualified Data.Text.IO         as TIO
import           Linear.V2            (V2 (..))
import           System.FilePath
import           Wrench.Animation
import           Wrench.AnimId
import           Wrench.ImageId
import           Wrench.Rectangle

data DataLine a = DataLineImage (ImageId,Rectangle a)
                | DataLineAnim (AnimId,Animation) deriving(Eq,Show)

readImageDataFromFile :: (Integral a,Bounded a) => MonadIO m => FilePath -> m [DataLine a]
readImageDataFromFile f = liftM readImageDataFromText ((liftIO . TIO.readFile) f)

readImageDataFromText :: (Integral a,Bounded a) => Text -> [DataLine a]
readImageDataFromText t =
  case parseOnly imageDataC t of
    Left e -> error $ "Parse error: " <> e
    Right r -> r

imageDataC :: (Integral a,Bounded a) => Parser [DataLine a]
imageDataC = sepBy1 imageDataLineC (char '\n')

intOrError :: (Integral i,Bounded i) => Scientific -> i
intOrError s =
  case toBoundedInteger s of
    Nothing -> error $ "scientific \"" <> show s <>"\" is not an integer"
    Just s' -> s'

--realFloatOrError :: RealFloat i => Scientific -> i
--realFloatOrError s = toRealFloat s

scientificBoundedInt :: (Integral i,Bounded i) => Parser i
scientificBoundedInt = intOrError <$> scientific

--scientificRealFloat :: RealFloat i => Parser i
--scientificRealFloat = realFloatOrError <$> scientific

point :: (Integral a,Bounded a) => Parser (V2 a)
point = V2 <$> scientificBoundedInt <*> (char ',' *> scientificBoundedInt)

rectangle :: (Integral a,Bounded a) => Parser (Rectangle a)
rectangle = rectFromPoints <$> point <*> (char ',' *> point)

imageDataLineC :: (Integral a,Bounded a) => Parser (DataLine a)
imageDataLineC = (char '>' *> (DataLineAnim <$> imageDataLineAnimC)) <|> (DataLineImage <$> imageDataLineImageC)

stringNotInClass :: String -> Parser Text
stringNotInClass s = pack <$> many1 (satisfy (notInClass s))

imageDataLineImageC :: (Integral a,Bounded a) => Parser (ImageId,Rectangle a)
imageDataLineImageC = (,) <$> stringNotInClass "=\n" <*> (char '=' *> rectangle)

imageDataLineAnimC :: Parser (AnimId,Animation)
imageDataLineAnimC = (,) <$> stringNotInClass "=\n" <*> (Animation <$> (char '=' *> scientificBoundedInt <* char '|') <*> sepBy1 (stringNotInClass  ",\n") (char ','))
