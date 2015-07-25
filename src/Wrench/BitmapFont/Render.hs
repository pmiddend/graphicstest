module Wrench.BitmapFont.Render(
    textToPicture
  , FontPrefix) where

import           ClassyPrelude                  hiding (last)
import           Control.Lens                   (ix, (^.), (^.), (^?!), _2)
import           Data.List                      (foldl, last)
import           Linear.V2
import           Wrench.BitmapFont.RenderResult
import           Wrench.ImageData
import           Wrench.Picture
import           Wrench.Rectangle
import           Wrench.SpriteIdentifier

charToText :: Char -> Text
charToText c = pack [c]

characterToImage :: SurfaceMap a -> FontPrefix -> Char -> Maybe SpriteIdentifier
characterToImage images prefix c =
  let
    identifier = prefix <> "_" <> (charToText c)
  in
    if identifier `member` images
    then Just identifier
    else error $ "invalid identifier " <> (unpack identifier)

type FontPrefix = Text

textToPicture :: Num unit => SurfaceMap a -> FontPrefix -> unit -> Text -> RenderResult unit float
textToPicture images prefix spacing text =
  let
    fontSize = fromIntegral <$> (images ^?! ix (prefix <> "_a")) ^. _2
    characterWidth = fontSize ^. rectWidth
    characterPictures = mapMaybe ((pictureSpriteTopLeft <$>) . (characterToImage images prefix)) (unpack text)
    characterPositionAdder [] _ = [V2 0 0]
    characterPositionAdder (x:xs) _ = (x + V2 (characterWidth + spacing) 0):x:xs
    characterPositions = reverse (foldl characterPositionAdder [] characterPictures)
  in
    RenderResult{
        _bfrrPicture = pictures (zipWith pictureTranslated characterPositions characterPictures)
      , _bfrrSize = V2 ((last characterPositions) ^. _x + characterWidth) (fontSize ^. rectHeight)
      }
