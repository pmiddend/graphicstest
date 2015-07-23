{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Wrench.Engine(
    withPlatform
  , wrenchRender
  , PlatformBackend
  ) where

import           Control.Lens                     ((&), (^.))
import           Control.Lens.Getter              (Getter, to)
import           Control.Lens.Setter              ((%~), (+~), (.~))
import           Control.Lens.TH                  (makeLenses)
import           Linear.Matrix                    (M33, (!*), (!*!))
import           Linear.V2                        (V2 (..), _x, _y)
import           Linear.V3                        (V3 (..))
import           Numeric.Lens                     (dividing)
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Event
import           Wrench.ImageData
import           Wrench.Internal.Picture
import           Wrench.MouseGrabMode
import           Wrench.Platform
import           Wrench.Point                     (Point)
import           Wrench.Rectangle
import           Wrench.RenderPositionMode
import           Wrench.WindowSize
#if defined(USE_SGE)
import           Wrench.Backends.Sge.SGEPlatform
#endif
#if defined(USE_SDL)
import           Wrench.Backends.Sdl.Sdl2Platform
#endif
import           ClassyPrelude                    hiding (FilePath, (</>))
import           Wrench.List                      (concatMapM)

-- TODO: Use Linear.Matrix.identity
eye3 :: Num a => M33 a
eye3 = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

-- toV3 :: Num a => V2 a -> V3 a
-- toV3 (V2 x y) = V3 x y 1

toV2 :: Getter (V3 a) (V2 a)
toV2 = to toV2'
  where toV2' (V3 x y _) = V2 x y

mkTranslation :: Num a => V2 a -> M33 a
mkTranslation p = V3 (V3 1 0 (p ^. _x)) (V3 0 1 (p ^. _y)) (V3 0 0 1)

mkRotation :: (Num a,Floating a) => a -> M33 a
mkRotation r = V3 (V3 cs (-sn) 0) (V3 sn cs 0) (V3 0 0 1)
  where cs = cos r
        sn = sin r

mkScale :: Num a => V2 a -> M33 a
mkScale p = V3 (V3 (p ^. _x) 0 0) (V3 0 (p ^. _y) 0) (V3 0 0 1)

-- mkTransformation :: FloatType -> Point -> M33 FloatType
-- mkTransformation r p = mkTranslation p !*! mkRotation r

data RenderState i j f a =
  RenderState { _rsTransformation :: M33 a
              , _rsSurfaceData    :: SurfaceMap i j
              , _rsFont           :: f
              , _rsColor          :: Color
              , _rsRotation       :: Radians a
              }

$(makeLenses ''RenderState)

data RenderOperation image font pos rads = RenderOperationSprite (SpriteInstance image pos rads)
                                         | RenderOperationText (TextInstance font pos)

opToSprite :: RenderOperation image font pos rads -> SpriteInstance image pos rads
opToSprite (RenderOperationSprite s) = s
opToSprite (RenderOperationText _) = error "Cannot extract sprite from text"

opToText :: RenderOperation image font pos rads -> TextInstance font pos
opToText (RenderOperationSprite _) = error "Cannot extract text from sprite"
opToText (RenderOperationText s) = s

executeOperationBatch :: Platform p => p -> [ RenderOperation (PlatformImage p) (PlatformFont p) pos rads ] -> IO ()
executeOperationBatch p ss@( RenderOperationSprite _ : _ ) = renderSprites p (map opToSprite ss)
executeOperationBatch p ss@( RenderOperationText _ : _ ) = renderText p (map opToText ss)
executeOperationBatch _ [] = return ()

equalOperation :: RenderOperation image font pos rads -> RenderOperation image font pos rads -> Bool
equalOperation (RenderOperationSprite _) (RenderOperationSprite _) = True
equalOperation (RenderOperationText _) (RenderOperationText _) = True
equalOperation _ _ = False

renderPicture :: RenderState font pos image a -> Picture pos rads-> IO [RenderOperation font image pos rads]
renderPicture rs p = case p of
  Blank -> return []
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

wrenchRender :: Num a => Platform p => p -> SurfaceMap (PlatformImage p) a -> PlatformFont p -> Maybe Color -> Picture a b -> IO ()
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

withPlatform :: WindowTitle -> WindowSize -> MouseGrabMode -> (PlatformBackend -> IO ()) -> IO ()
#if defined(USE_SGE)
withPlatform = withSGEPlatform
#else
withPlatform = withSdlPlatform
#endif
