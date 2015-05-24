{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Wrench.Parsec(
    int
  , safeParseFromFile
  , floatType
  , safeParseFromText
  ) where

import           Text.Parsec         (digit, many1)
import           Text.Parsec.Prim    (ParsecT, Stream,parse)
import           Text.Parsec.String  (Parser, parseFromFile)
import           Text.Read           (read)
import           Wrench.FloatType
import ClassyPrelude

int :: Stream s m Char => ParsecT s u m Int
int = rd <$> many1 digit
  where rd = read :: String -> Int

floatType :: Stream s m Char => ParsecT s u m FloatType
floatType = rd <$> many1 digit
  where rd = read :: String -> FloatType

safeParseFromFile :: Parser a -> String -> IO a
safeParseFromFile p f = do
  result <- parseFromFile p f
  return $ case result of
    Left e -> error $ "Parse error: " ++ show e
    Right r -> r

safeParseFromText :: Parser a -> Text -> a
safeParseFromText p text = case parse p "" (unpack text) of
  Left e -> error $ "Parse error: " ++ show e
  Right r -> r
