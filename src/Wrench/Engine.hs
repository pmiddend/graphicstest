{-|
Module      : Wrench.Engine
Description : Functions for initializing a Platform and drawing Pictures
Maintainer  : pmidden@secure.mailbox.org
-}
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
                                                   (%~), (&), (*~), (+~), (.~),
                                                   (^.), (^..))
import           Linear.Matrix                    (M33, (!*), (!*!))
import           Linear.V2                        (V2 (..), _x, _y)
import           Linear.V3                        (V3 (..))
import           Wrench.Angular
import           Wrench.Color
import           Wrench.ImageData
import           Wrench.Internal.Picture
import           Wrench.MouseGrabMode
import           Wrench.Platform
import           Wrench.Rectangle
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

mkRotation :: (Floating a) => a -> M33 a
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
              , _rsScale          :: V2 float
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

renderPicture :: (Show float,RealFrac float,Floating float) => RenderState float font image -> Picture float float -> IO [RenderOperation float float font image]
renderPicture rs p = case p of
  Blank -> return []
  --Text s -> renderText (rs ^. rsPlatform) (rs ^. rsFont) (s) (rs ^. rsColor) (((rs ^. rsTransformation) !* V3 0 0 1) ^. toV2)
  Text s -> return [RenderOperationText (TextInstance (rs ^. rsFont) s (rs ^. rsColor) (((rs ^. rsTransformation) !* V3 0 0 1) ^. toV2))]
  InColor color picture -> renderPicture (rs & rsColor .~ color) picture
  Pictures ps -> concatMapM (renderPicture rs) ps
  Translate point picture -> renderPicture (rs & rsTransformation %~ (!*! mkTranslation point)) picture
  Rotate r picture -> renderPicture (rs & ((rsRotation +~ r) . (rsTransformation %~ (!*! mkRotation (r ^. _Radians))))) picture
  Scale s picture -> renderPicture (rs & ((rsScale *~ s) . (rsTransformation %~ (!*! mkScale s)))) picture
  Sprite identifier resampledSize -> do
    let (image,srcRect) = findSurfaceUnsafe (rs ^. rsSurfaceData) identifier
        m = rs ^. rsTransformation
--        origin = (m !* (negate ((/2) <$> (resampledSize ^. toV3)))) ^. toV2
        origin = ((m !* V3 0 0 1) ^. toV2) - resampledSize / 2
        rot = rs ^. rsRotation
        destRect = rectFromOriginAndDim origin ((rs ^. rsScale) * resampledSize)
    return [RenderOperationSprite (SpriteInstance image (srcRect ^. to (fmap fromIntegral)) destRect rot (rs ^. rsColor))]

-- | Render a 'Picture' using integer coordinates using the given 'Platform'
wrenchRender :: forall real int p.(Show real,RealFrac real,Integral int,Floating real,Platform p) => p
             -> SurfaceMap (PlatformImage p) -- ^ Surfaces to use for the sprite identifiers
             -> PlatformFont p -- ^ Font to use for font nodes (can be undefined if the picture contains no fonts)
             -> Maybe Color -- ^ Screen clear color. If this is not given, the screen is not cleared
             -> Picture int real -- ^ The picture to render
             -> IO ()
wrenchRender platform surfaceMap font backgroundColor outerPicture = do
  renderBegin platform
  maybe (return ()) (renderClear platform) backgroundColor
  let
    initialRenderState = RenderState {
        _rsTransformation = eye3
      , _rsSurfaceData = surfaceMap
      , _rsFont = font
      , _rsColor = colorsWhite
      , _rsRotation = 0
      , _rsScale = V2 1 1
    }
  operations <- renderPicture initialRenderState (first fromIntegral outerPicture)
  mapM_ (executeOperationBatch platform . (mapRoUnit (floor :: real -> Int) <$>)) (groupBy equalOperation operations)
  renderFinish platform

#if defined(USE_SGE)
type PlatformBackend = SGEPlatform
#else
type PlatformBackend = SDL2Platform
#endif

-- | Initialize platform, create a window and execute the function with the platform
withPlatform :: WindowTitle
             -> WindowSize -- ^ Constant or dynamic window size
             -> MouseGrabMode -- ^ Whether to grab the mouse and hide the cursor or not
             -> (PlatformBackend -> IO ()) -- ^ "Main loop" function
             -> IO ()
#if defined(USE_SGE)
withPlatform = withSGEPlatform
#else
withPlatform = withSdlPlatform
#endif
