module Wrench.Picture(
    Picture
  , pictureText
  , pictureSprite
  , pictureSpriteResampled
  , pictureBlank
  , pictureInColor
  , pictureTranslated
  , pictureRotated
  , pictures
  , pictureScaled
  , pictureSpriteCentered
  , pictureSpriteTopLeft
  ) where

import           ClassyPrelude
import           Linear.V2                 (V2)
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Internal.Picture
import           Wrench.RenderPositionMode
import           Wrench.SpriteIdentifier

pictureText :: Text -> Picture a
pictureText = Text

pictureSpriteResampled :: SpriteIdentifier -> RenderPositionMode -> (V2 a) -> Picture a
pictureSpriteResampled identifier positionMode newSize = Sprite identifier positionMode (Just newSize)

pictureSprite :: SpriteIdentifier -> RenderPositionMode -> Picture a
pictureSprite identifier positionMode = Sprite identifier positionMode Nothing

pictureSpriteCentered :: SpriteIdentifier -> Picture a
pictureSpriteCentered identifier = Sprite identifier RenderPositionCenter Nothing

pictureSpriteTopLeft :: SpriteIdentifier -> Picture a
pictureSpriteTopLeft identifier = Sprite identifier RenderPositionTopLeft Nothing

pictureBlank :: Picture a
pictureBlank = Blank

pictureInColor :: Color -> Picture a -> Picture a
pictureInColor = InColor

pictureTranslated :: (V2 a) -> Picture a -> Picture a
pictureTranslated = Translate

pictureRotated :: Radians -> Picture a -> Picture a
pictureRotated = Rotate

pictureScaled :: (V2 a) -> Picture a -> Picture a
pictureScaled = Scale

pictures :: [Picture a] -> Picture a
pictures = Pictures

