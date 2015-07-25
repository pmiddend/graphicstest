module Wrench.Picture(Picture,pictureText,pictureSprite,pictureSpriteResampled,pictureBlank,pictureInColor,pictureTranslated,pictureRotated,pictures,pictureScaled,pictureSpriteCentered,pictureSpriteTopLeft) where

import           ClassyPrelude
import           Linear.V2
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Internal.Picture
import           Wrench.RenderPositionMode
import           Wrench.SpriteIdentifier

pictureText :: Text -> Picture unit float
pictureText = Text

pictureSpriteResampled :: SpriteIdentifier -> RenderPositionMode -> V2 unit -> Picture unit float
pictureSpriteResampled identifier positionMode newSize = Sprite identifier positionMode (Just newSize)

pictureSprite :: SpriteIdentifier -> RenderPositionMode -> Picture unit float
pictureSprite identifier positionMode = Sprite identifier positionMode Nothing

pictureSpriteCentered :: SpriteIdentifier -> Picture unit float
pictureSpriteCentered identifier = Sprite identifier RenderPositionCenter Nothing

pictureSpriteTopLeft :: SpriteIdentifier -> Picture unit float
pictureSpriteTopLeft identifier = Sprite identifier RenderPositionTopLeft Nothing

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

