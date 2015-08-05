{-|
Module      : Wrench.BitmapFont.Render
Description : Fucntions for rendering bitmap fonts
Maintainer  : pmidden@secure.mailbox.org

Bitmap fonts are nothing special in Wrench. They leverage the same
texture atlasing system that is used for normal images and rendering
a bitmap font is the same as rendering a collection of sprites. The
result is a 'Wrench.Picture' which you can move, rotate, etc..

The idea is to load an atlased image file containing the glyphs,
whose identifiers are prefixed by the font name, and then call
'textToPicture' to create a picture for a given input text.
-}
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

-- | Type synonym to make the function signature for 'textToPicture' prettier
type FontPrefix = Text

textToPicture :: SurfaceMap a -- ^ Surface map containing the glyphs
              -> FontPrefix   -- ^ Prefix to map characters to image identifiers
              -> Int -- ^ Spacing between characters - can be negative 
              -> Text -- ^ The text to render
              -> RenderResult Int float
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
