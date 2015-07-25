{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
module Wrench.Engine(
    withPlatform
  , wrenchRender
  , PlatformBackend
  ) where

import           Control.Lens                     (Getter, makeLenses,
                                                   makePrisms, to, traversed,
                                                   (%~), (&), (+~), (.~), (^.),
                                                   (^..))
import           Linear.Matrix                    (M33, (!*), (!*!))
import           Linear.V2                        (V2 (..), _x, _y)
import           Linear.V3                        (V3 (..))
import           Numeric.Lens                     (dividing)
import           Wrench.Angular
import           Wrench.Color
import           Wrench.ImageData
import           Wrench.Internal.Picture
import           Wrench.MouseGrabMode
import           Wrench.Platform
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

mkRotation :: (Floating a,Num a) => a -> M33 a
mkRotation r = V3 (V3 cs (-sn) 0) (V3 sn cs 0) (V3 0 0 1)
  where cs = cos r
        sn = sin r

mkScale :: Num a => V2 a -> M33 a
mkScale p = V3 (V3 (p ^. _x) 0 0) (V3 0 (p ^. _y) 0) (V3 0 0 1)

-- mkTransformation :: FloatType -> Point -> M33 FloatType
-- mkTransformation r p = mkTranslation p !*! mkRotation r

data RenderState float image font =
  RenderState { _rsTransformation :: M33 float
              , _rsSurfaceData    :: SurfaceMap image
              , _rsFont           :: font
              , _rsColor          :: Color
              , _rsRotation       :: Radians float
              }

$(makeLenses ''RenderState)

data RenderOperation unit float image font =
    RenderOperationSprite (SpriteInstance unit float image)
  | RenderOperationText (TextInstance unit font)

mapRoUnit :: (a -> b) -> RenderOperation a float image font -> RenderOperation b float image font
mapRoUnit f (RenderOperationSprite si) = RenderOperationSprite (mapSpriteInstanceUnit f si)
mapRoUnit f (RenderOperationText si) = RenderOperationText (mapTextInstanceUnit f si)

$(makePrisms ''RenderOperation)

executeOperationBatch :: (Platform p, Real real, Integral unit, Floating real) => p -> [RenderOperation unit real (PlatformImage p) (PlatformFont p)] -> IO ()
executeOperationBatch p ss@( RenderOperationSprite _ : _ ) = renderSprites p (ss ^.. traversed . _RenderOperationSprite)
executeOperationBatch p ss@( RenderOperationText _ : _ ) = renderText p (ss ^.. traversed . _RenderOperationText)
executeOperationBatch _ [] = return ()

equalOperation :: RenderOperation unit float image font -> RenderOperation unit float image font -> Bool
equalOperation (RenderOperationSprite _) (RenderOperationSprite _) = True
equalOperation (RenderOperationText _) (RenderOperationText _) = True
equalOperation _ _ = False

renderPicture :: (RealFrac float,Floating float) => RenderState float font image -> Picture float float -> IO [RenderOperation float float font image]
renderPicture rs p = case p of
  Blank -> return []
  --Text s -> renderText (rs ^. rsPlatform) (rs ^. rsFont) (s) (rs ^. rsColor) (((rs ^. rsTransformation) !* V3 0 0 1) ^. toV2)
  Text s -> return [RenderOperationText (TextInstance (rs ^. rsFont) s (rs ^. rsColor) (((rs ^. rsTransformation) !* V3 0 0 1) ^. toV2))]
  InColor color picture -> renderPicture (rs & rsColor .~ color) picture
  Pictures ps -> concatMapM (renderPicture rs) ps
  Translate point picture -> renderPicture (rs & rsTransformation %~ (!*! mkTranslation point)) picture
  Rotate r picture -> renderPicture (rs & ((rsRotation +~ r) . (rsTransformation %~ (!*! mkRotation (r ^. _Radians))))) picture
  Scale s picture -> renderPicture (rs & rsTransformation %~ (!*! mkScale s)) picture
  Sprite identifier positionMode resampledSize -> do
    let (image,srcRect) = findSurfaceUnsafe (rs ^. rsSurfaceData) identifier
        m = rs ^. rsTransformation
        origin = (m !* V3 0 0 1) ^. toV2
        spriteDim = fromMaybe (srcRect ^. rectDimensions . to (fmap fromIntegral)) resampledSize
        pos = case positionMode of
          RenderPositionCenter -> origin - (spriteDim ^. dividing 2)
          RenderPositionTopLeft -> origin
        rot = rs ^. rsRotation
        destRect = rectFromPoints pos (pos + spriteDim)
    return [RenderOperationSprite (SpriteInstance image (srcRect ^. to (fmap fromIntegral)) destRect rot)]

wrenchRender :: forall real int.(RealFrac real,Integral int,Floating real,Real real) => Platform p => p -> SurfaceMap (PlatformImage p) -> PlatformFont p -> Maybe Color -> Picture int real -> IO ()
wrenchRender platform surfaceMap font backgroundColor outerPicture = do
  renderBegin platform
  maybe (return ()) (renderClear platform) backgroundColor
  operations <- renderPicture (RenderState eye3 surfaceMap font (fromMaybe colorsWhite backgroundColor) 0) (first fromIntegral outerPicture)
  mapM_ (executeOperationBatch platform . (mapRoUnit (floor :: real -> Int) <$>)) (groupBy equalOperation operations)
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
