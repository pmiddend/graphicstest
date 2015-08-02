module Wrench.Picture(
    Picture
  , pictureText
  , pictureScaled
  , pictureSprite
  , pictureBlank
  , pictureInColor
  , pictureTranslated
  , pictureRotated
  , pictures) where

import           ClassyPrelude
import           Linear.V2
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Internal.Picture
import           Wrench.SpriteIdentifier

pictureText :: Text -> Picture unit float
pictureText = Text

pictureSprite :: SpriteIdentifier -> V2 unit -> Picture unit float
pictureSprite = Sprite

pictureBlank :: Picture unit float
pictureBlank = Blank

pictureInColor :: Color -> Picture unit float -> Picture unit float
pictureInColor = InColor

pictureTranslated :: V2 unit -> Picture unit float -> Picture unit float
pictureTranslated = Translate

pictureRotated :: Radians float -> Picture unit float -> Picture unit float
pictureRotated = Rotate

pictureScaled :: V2 unit -> Picture unit float -> Picture unit float
pictureScaled = Scale

pictures :: [Picture unit float] -> Picture unit float
pictures = Pictures

