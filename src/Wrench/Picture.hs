module Wrench.Picture(Picture,pictureLine,pictureText,pictureSprite,pictureSpriteResampled,pictureBlank,pictureInColor,pictureTranslated,pictureRotated,pictures,pictureScaled,pictureSpriteCentered,pictureSpriteTopLeft) where

import Wrench.Internal.Picture
import Wrench.Point
import Wrench.Color
import Wrench.SpriteIdentifier
import Wrench.Angular
import Wrench.RenderPositionMode
import ClassyPrelude

pictureLine :: Point -> Point -> Picture
pictureLine = Line

pictureText :: Text -> Picture
pictureText = Text

pictureSpriteResampled :: SpriteIdentifier -> RenderPositionMode -> Point -> Picture
pictureSpriteResampled identifier positionMode newSize = Sprite identifier positionMode (Just newSize)

pictureSprite :: SpriteIdentifier -> RenderPositionMode -> Picture
pictureSprite identifier positionMode = Sprite identifier positionMode Nothing

pictureSpriteCentered :: SpriteIdentifier -> Picture
pictureSpriteCentered identifier = Sprite identifier RenderPositionCenter Nothing

pictureSpriteTopLeft :: SpriteIdentifier -> Picture
pictureSpriteTopLeft identifier = Sprite identifier RenderPositionTopLeft Nothing

pictureBlank :: Picture
pictureBlank = Blank

pictureInColor :: Color -> Picture -> Picture
pictureInColor = InColor

pictureTranslated :: Point -> Picture -> Picture
pictureTranslated = Translate

pictureRotated :: Radians -> Picture -> Picture
pictureRotated = Rotate

pictureScaled :: Point -> Picture -> Picture
pictureScaled = Scale

pictures :: [Picture] -> Picture
pictures = Pictures

