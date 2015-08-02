{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Wrench.ImageParser(DataLine(..),readImageDataFromFile,readImageDataFromText) where

import           ClassyPrelude          hiding (FilePath, (</>))
import           Data.Attoparsec.Text   (Parser, char, many1, notInClass,
                                         parseOnly, satisfy, scientific, sepBy1)
import           Data.Scientific        (Scientific, toBoundedInteger)
import qualified Data.Text.IO           as TIO
import           Linear.V2              (V2 (..))
import           System.FilePath
import           Wrench.Animation
import           Wrench.AnimIdentifier
import           Wrench.ImageIdentifier
import           Wrench.Rectangle
import           Wrench.Time

data DataLine = DataLineImage (ImageIdentifier,Rectangle Int)
              | DataLineAnim (AnimIdentifier,Animation) deriving(Eq,Show)

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

{-
realFloatOrError :: RealFloat i => Scientific -> i
realFloatOrError s = toRealFloat s

scientificRealFloat :: RealFloat i => Parser i
scientificRealFloat = realFloatOrError <$> scientific
-}

scientificBoundedInt :: (Integral i,Bounded i) => Parser i
scientificBoundedInt = intOrError <$> scientific

point :: Parser (V2 Int)
point = V2 <$> scientificBoundedInt <*> (char ',' *> scientificBoundedInt)

rectangle :: Parser (Rectangle Int)
rectangle = rectFromPoints <$> point <*> (char ',' *> point)

imageDataLineC :: Parser DataLine
imageDataLineC = (char '>' *> (DataLineAnim <$> imageDataLineAnimC)) <|> (DataLineImage <$> imageDataLineImageC)

stringNotInClass :: String -> Parser Text
stringNotInClass s = pack <$> many1 (satisfy (notInClass s))

imageDataLineImageC :: Parser (ImageIdentifier,Rectangle Int)
imageDataLineImageC = (,) <$> stringNotInClass "=\n" <*> (char '=' *> rectangle)

imageDataLineAnimC :: Parser (AnimIdentifier,Animation)
imageDataLineAnimC = (,) <$> stringNotInClass "=\n" <*> (Animation <$> (char '=' *> ((fromMilliseconds . (fromIntegral :: Int -> Double)) <$> scientificBoundedInt) <* char '|') <*> sepBy1 (stringNotInClass  ",\n") (char ','))
