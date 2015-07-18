{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Wrench.Engine(
    Event(..)
  , withPlatform
  , wrenchRender
  , PlatformBackend
  ) where

import           Control.Lens              ((&), (^.))
import           Control.Lens.Getter       (Getter, to)
import           Control.Lens.Setter       ((%~), (+~), (.~))
import           Control.Lens.TH           (makeLenses)
import           Linear.Matrix             (M33, (!*), (!*!))
import           Linear.V2                 (V2 (..), _x, _y)
import           Linear.V3                 (V3 (..))
import           Numeric.Lens              (dividing)
import           Wrench.Angular
import           Wrench.Color
import           Wrench.WindowSize
import           Wrench.Event
import           Wrench.Rectangle
import           Wrench.FloatType          (FloatType)
import           Wrench.ImageData
import           Wrench.Internal.Picture
import           Wrench.Platform
import           Wrench.Point              (Point)
import           Wrench.RenderPositionMode
#if defined(USE_SGE)
import           Wrench.Backends.Sge.SGEPlatform
#endif
#if defined(USE_SDL)
import           Wrench.Backends.Sdl.Sdl2Platform
#endif
import           ClassyPrelude hiding(FilePath,(</>))
import Wrench.List(concatMapM)

-- TODO: Use Linear.Matrix.identity
eye3 :: Num a => M33 a
eye3 = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

-- toV3 :: Num a => V2 a -> V3 a
-- toV3 (V2 x y) = V3 x y 1

toV2 :: Getter (V3 a) (V2 a)
toV2 = to toV2'
  where toV2' (V3 x y _) = V2 x y

mkTranslation :: Point -> M33 FloatType
mkTranslation p = V3 (V3 1 0 (p ^. _x)) (V3 0 1 (p ^. _y)) (V3 0 0 1)

mkRotation :: FloatType -> M33 FloatType
mkRotation r = V3 (V3 cs (-sn) 0) (V3 sn cs 0) (V3 0 0 1)
  where cs = cos r
        sn = sin r

mkScale :: Point -> M33 FloatType
mkScale p = V3 (V3 (p ^. _x) 0 0) (V3 0 (p ^. _y) 0) (V3 0 0 1)

-- mkTransformation :: FloatType -> Point -> M33 FloatType
-- mkTransformation r p = mkTranslation p !*! mkRotation r

data RenderState i f = RenderState { _rsTransformation :: M33 FloatType
                                   , _rsSurfaceData    :: SurfaceMap i
                                   , _rsFont           :: f
                                   , _rsColor          :: Color
                                   , _rsRotation       :: Radians
                                 }

$(makeLenses ''RenderState)

data RenderOperation image font = RenderOperationSprite (SpriteInstance image)
                                | RenderOperationText (TextInstance font)

opToSprite :: RenderOperation image font -> SpriteInstance image
opToSprite (RenderOperationSprite s) = s
opToSprite (RenderOperationText _) = error "Cannot extract sprite from text"

opToText :: RenderOperation image font -> TextInstance font
opToText (RenderOperationSprite _) = error "Cannot extract text from sprite"
opToText (RenderOperationText s) = s

executeOperationBatch :: Platform p => p -> [ RenderOperation (PlatformImage p) (PlatformFont p) ] -> IO ()
executeOperationBatch p ss@( RenderOperationSprite _ : _ ) = renderSprites p (map opToSprite ss)
executeOperationBatch p ss@( RenderOperationText _ : _ ) = renderText p (map opToText ss)
executeOperationBatch _ [] = return ()

equalOperation :: RenderOperation image font -> RenderOperation image font -> Bool
equalOperation (RenderOperationSprite _) (RenderOperationSprite _) = True
equalOperation (RenderOperationText _) (RenderOperationText _) = True
equalOperation _ _ = False

renderPicture :: RenderState font image -> Picture -> IO [RenderOperation font image]
renderPicture rs p = case p of
  Blank -> return []
  Line _ _ -> return [] -- TODO
  --Text s -> renderText (rs ^. rsPlatform) (rs ^. rsFont) (s) (rs ^. rsColor) (((rs ^. rsTransformation) !* V3 0 0 1) ^. toV2)
  Text s -> return [RenderOperationText (TextInstance (rs ^. rsFont) s (rs ^. rsColor) (((rs ^. rsTransformation) !* V3 0 0 1) ^. toV2) )]
  InColor color picture -> renderPicture (rs & rsColor .~ color) picture
  Pictures ps -> concatMapM (renderPicture rs) ps
  Translate point picture -> renderPicture (rs & rsTransformation %~ (!*! mkTranslation point)) picture
  Rotate r picture -> renderPicture (rs & ((rsRotation +~ r) . (rsTransformation %~ (!*! mkRotation (r ^. getRadians))))) picture
  Scale s picture -> renderPicture (rs & rsTransformation %~ (!*! mkScale s)) picture
  Sprite identifier positionMode resampledSize -> do
    let (image,srcRect) = findSurfaceUnsafe (rs ^. rsSurfaceData) identifier
        m = rs ^. rsTransformation
        origin = (m !* V3 0 0 1) ^. toV2
        spriteDim = fromMaybe (srcRect ^. rectDimensions) resampledSize
        pos = case positionMode of
          RenderPositionCenter -> origin - (spriteDim ^. dividing 2)
          RenderPositionTopLeft -> origin
        rot = rs ^. rsRotation
        destRect = rectFromPoints pos (pos + spriteDim)
    return [RenderOperationSprite (SpriteInstance image srcRect destRect rot)]

wrenchRender :: Platform p => p -> SurfaceMap (PlatformImage p) -> PlatformFont p -> Maybe Color -> Picture -> IO ()
wrenchRender platform surfaceMap font backgroundColor outerPicture = do
  renderBegin platform
  maybe (return ()) (renderClear platform) backgroundColor
  operations <- renderPicture (RenderState eye3 surfaceMap font (fromMaybe colorsWhite backgroundColor) 0) outerPicture
  mapM_ (executeOperationBatch platform) (groupBy equalOperation operations)
  renderFinish platform

#if defined(USE_SGE)
type PlatformBackend = SGEPlatform
#else
type PlatformBackend = SDL2Platform
#endif

withPlatform :: WindowTitle -> WindowSize -> (PlatformBackend -> IO ()) -> IO ()
#if defined(USE_SGE)
withPlatform = withSGEPlatform
#else
withPlatform = withSdlPlatform
#endif
