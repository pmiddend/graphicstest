module Wrench.BitmapFont(textToPicture) where

import ClassyPrelude
import Wrench.Point
import Wrench.SpriteIdentifier
import Wrench.Picture
import Wrench.Rectangle
import Wrench.ImageMap
import Control.Lens((^?!),at,(^.),ix)

type FontPrefix = Text

charToText :: Char -> Text
charToText c = pack [c]

characterToImage :: ImageMap -> FontPrefix -> Char -> Maybe SpriteIdentifier
characterToImage images prefix c =
  let
    identifier = prefix <> "_" <> (charToText c)
  in
    if identifier `member` images
    then Just identifier
    else Nothing
      

textToPicture :: ImageMap -> FontPrefix -> Text -> Picture
textToPicture images prefix text =
  let
    fontSize = images ^?! ix (prefix <> "_a")
    characterPictures = mapMaybe ((pictureSpriteTopLeft <$>) . (characterToImage images prefix)) (unpack text)
    characterPositions = foldr (V2 0 0) (map (const () characterPictures))
  in
    pictures (zipWith pictureTranslated characterPictures characterPositions)
  

