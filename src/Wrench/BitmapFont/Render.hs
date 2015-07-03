module Wrench.BitmapFont.Render(
    textToPicture
  , FontPrefix
  , Spacing) where

import Wrench.BitmapFont.RenderResult
import ClassyPrelude hiding(last)
import Wrench.Point
import Wrench.FloatType
import Wrench.SpriteIdentifier
import Wrench.Picture
import Wrench.Rectangle
import Wrench.ImageData
import Linear.V2
import Control.Lens((^?!),at,(^.),ix,(^..),(^.),_2)
import Data.List(foldl,last)

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
type Spacing = FloatType

textToPicture :: SurfaceMap a -> FontPrefix -> Spacing -> Text -> RenderResult
textToPicture images prefix spacing text =
  let
    fontSize = (images ^?! ix (prefix <> "_a")) ^. _2
    characterWidth = fontSize ^. rectDimensions . _x
    characterPictures = mapMaybe ((pictureSpriteTopLeft <$>) . (characterToImage images prefix)) (unpack text)
    characterPositionAdder [] _ = [V2 0 0]
    characterPositionAdder (x:xs) _ = (x + V2 (characterWidth + spacing) 0):x:xs
    characterPositions = reverse (foldl characterPositionAdder [] characterPictures)
  in
    RenderResult{
        _bfrrPicture = pictures (zipWith pictureTranslated characterPositions characterPictures)
      , _bfrrSize = V2 ((last characterPositions) ^. _x + characterWidth) (fontSize ^. rectDimensions . _y)
      }
