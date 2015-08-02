module Wrench.BitmapFont.Render(
    textToPicture
  , FontPrefix) where

import           ClassyPrelude                  hiding (last)
import           Control.Lens                   (ix, (^.), (^.), (^?!), _2)
import           Control.Lens                   (view)
import           Data.List                      (foldl, last)
import           Linear.V2
import           Wrench.BitmapFont.RenderResult
import           Wrench.ImageData
import           Wrench.Picture
import           Wrench.Rectangle
import           Wrench.SpriteIdentifier

charToText :: Char -> Text
charToText c = pack [c]

characterToImage :: SurfaceMap a -> FontPrefix -> Char -> Maybe (SpriteIdentifier,V2 Int)
characterToImage images prefix c =
  let
    identifier = prefix <> "_" <> (charToText c)
  in
    maybe (error ("invalid identifier " <> (unpack identifier))) (Just . bimap (const identifier) (view rectDimensions)) (identifier `lookup` images)

type FontPrefix = Text

textToPicture :: SurfaceMap a -> FontPrefix -> Int -> Text -> RenderResult Int float
textToPicture images prefix spacing text =
  let
    fontSize = fromIntegral <$> images ^?! ix (prefix <> "_a") ^. _2
    characterWidth = fontSize ^. rectWidth
    characterPictures = mapMaybe ((uncurry pictureSprite <$>) . (characterToImage images prefix)) (unpack text)
    characterPositionAdder [] _ = [V2 0 0]
    characterPositionAdder (x:xs) _ = (x + V2 (characterWidth + spacing) 0):x:xs
    characterPositions = reverse (foldl characterPositionAdder [] characterPictures)
  in
    RenderResult{
        _bfrrPicture = pictures (zipWith pictureTranslated characterPositions characterPictures)
      , _bfrrSize = V2 ((last characterPositions) ^. _x + characterWidth) (fontSize ^. rectHeight)
      }
