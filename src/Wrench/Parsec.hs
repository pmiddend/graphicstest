{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Wrench.Parsec(
    int
  , safeParseFromFile
  , floatType
  ) where

import           Control.Applicative ((<$>))
import           Control.Monad       (return)
import           Data.Either         (Either (Left, Right))
import           Data.Function       (($))
import           Data.Int            (Int)
import           Data.List           ((++))
import           Data.String         (String)
import           Prelude             (Char, error)
import           System.IO           (IO)
import           Text.Parsec         (digit, many1)
import           Text.Parsec.Prim    (ParsecT, Stream)
import           Text.Parsec.String  (Parser, parseFromFile)
import           Text.Read           (read)
import           Text.Show           (show)
import           Wrench.FloatType

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
