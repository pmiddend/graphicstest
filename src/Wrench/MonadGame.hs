{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Wrench.MonadGame(
    MonadGame(..)
  , runGame
  , glookupAnimUnsafe
  , glookupImageRectangleUnsafe
  , MonadGameBackend
  ) where

import           ClassyPrelude                  hiding (FilePath, (</>), threadDelay)
import           Control.Lens                   ((^.))
import           Control.Monad.Random           (MonadRandom (..), RandT,
                                                 evalRandT)
import           Control.Monad.State.Strict     (MonadState, StateT, evalStateT,
                                                 get, gets, modify, put)
import           Control.Monad.Writer.Lazy      (WriterT)
import qualified Data.Map.Strict                as M
import           Data.Maybe                     (fromJust)
import qualified Data.Set                       as S
import           Linear.V2
import           System.FilePath
import           System.Random                  (StdGen, getStdGen)
import           Wrench.Animation
import           Wrench.AnimIdentifier
import           Wrench.AudioData
import           Wrench.BitmapFont.Render
import           Wrench.BitmapFont.RenderResult
import           Wrench.Color
import           Wrench.Engine
import           Wrench.Event
import           Wrench.ImageData
import           Wrench.Keydowns
import           Wrench.KeyMovement
import           Wrench.MediaData
import           Wrench.MouseGrabMode
import           Wrench.Picture
import           Wrench.Platform                (Platform)
import qualified Wrench.Platform                as P
import           Wrench.PlayMode
import           Wrench.Rectangle
import           Wrench.RenderBlockMode
import           Wrench.Time
import           Wrench.WindowSize

class MonadGame m where
  gpollEvents :: m [Event]
  gupdateTicks :: Double -> m ()
  gplaySound :: SoundIdentifier -> m ()
  gupdateKeydowns :: [Event] -> m ()
  gcurrentTicks :: m TimeTicks
  grenderText :: FontPrefix -> Int -> Text -> m (RenderResult Int float)
  gcurrentTimeDelta :: m TimeDelta
  gcurrentKeydowns :: m Keydowns
  gviewportSize :: m (V2 Int)
  grender :: (Show float,Floating float,RealFrac float,Integral unit) => Picture unit float -> m ()
  glookupAnim :: AnimIdentifier -> m (Maybe Animation)
  glookupImageRectangle :: ImageIdentifier -> m (Maybe (Rectangle Int))

glookupAnimUnsafe :: (Functor m,MonadGame m) => AnimIdentifier -> m Animation
glookupAnimUnsafe anim = fromJust <$> glookupAnim anim

glookupImageRectangleUnsafe :: (Functor m,MonadGame m) => ImageIdentifier -> m (Rectangle Int)
glookupImageRectangleUnsafe im = fromJust <$> glookupImageRectangle im

data GameData p = GameData {
    gdSurfaces        :: SurfaceMap (P.PlatformImage p)
  , gdSounds          :: SoundMap (P.PlatformAudioBuffer p)
  , gdSources         :: [P.PlatformAudioSource p]
  , gdAnims           :: AnimMap
  , gdPlatform        :: p
  , gdBackgroundColor :: Maybe Color
  , gdCurrentTicks    :: !TimeTicks
  , gdTimeDelta       :: !TimeDelta
  , gdKeydowns        :: !Keydowns
  , gdFont            :: P.PlatformFont p
  , gdRenderBlockMode :: !RenderBlockMode
  , gdLastRender      :: !TimeTicks
  }

newtype GameDataM p a = GameDataM {
  runGameData :: RandT StdGen (StateT (GameData p) IO) a
  } deriving(Monad,MonadRandom,MonadIO,MonadState (GameData p),Applicative,Functor)

type MonadGameBackend a = GameDataM PlatformBackend a

instance (Monad m,MonadGame m) => MonadGame (StateT n m) where
  gpollEvents = lift gpollEvents
  gplaySound s = lift (gplaySound s)
  grenderText p s t = lift (grenderText p s t)
  gupdateTicks s = lift (gupdateTicks s)
  gupdateKeydowns e = lift (gupdateKeydowns e)
  gcurrentTicks = lift gcurrentTicks
  gviewportSize = lift gviewportSize
  gcurrentTimeDelta = lift gcurrentTimeDelta
  gcurrentKeydowns = lift gcurrentKeydowns
  grender p = lift (grender p)
  glookupAnim aid = lift (glookupAnim aid)
  glookupImageRectangle i = lift (glookupImageRectangle i)

instance (Monad m,MonadGame m,Monoid w) => MonadGame (WriterT w m) where
  gpollEvents = lift gpollEvents
  gupdateTicks s = lift (gupdateTicks s)
  grenderText p s t = lift (grenderText p s t)
  gplaySound s = lift (gplaySound s)
  gupdateKeydowns e = lift (gupdateKeydowns e)
  gviewportSize = lift gviewportSize
  gcurrentTicks = lift gcurrentTicks
  gcurrentTimeDelta = lift gcurrentTimeDelta
  gcurrentKeydowns = lift gcurrentKeydowns
  grender p = lift (grender p)
  glookupAnim aid = lift (glookupAnim aid)
  glookupImageRectangle i = lift (glookupImageRectangle i)

instance Platform p => MonadGame (GameDataM p) where
  gplaySound s = do
    p <- gets gdPlatform
    sounds <- gets gdSounds
    case s `M.lookup` sounds of
      Nothing -> error $ "Sound " <> unpack s <> " not found"
      Just buffer -> do
        source <- liftIO (P.playBuffer p buffer PlayModeOnce)
        -- TODO: This might be cleaner with lenses
        sources <- gets gdSources
        sourcesWithState <- traverse (\ts -> (liftIO (P.sourceIsStopped p ts)) >>= (\stopState -> return (ts,stopState))) sources
        mapM_ (\ts -> liftIO (P.freeSource p (fst ts))) (filter snd sourcesWithState)
        modify (\oldState -> oldState { gdSources = source : (map fst . filter (not . snd) $ sourcesWithState) })
  grenderText p s t = do
    surfaces <- gets gdSurfaces
    return (textToPicture surfaces p s t)
  gviewportSize = do
    p <- gets gdPlatform
    liftIO (P.viewportSize p)
  gpollEvents = do
    p <- gets gdPlatform
    liftIO $ P.pollEvents p
  gupdateTicks multiplier = do
    oldTicks <- gets gdCurrentTicks
    newTicks <- liftIO getTicks
    s <- get
    put s {
        gdCurrentTicks = newTicks,
        gdTimeDelta = fromSeconds multiplier * (newTicks `tickDelta` oldTicks)
        }
  gupdateKeydowns events = do
    oldKeydowns <- gets gdKeydowns
    s <- get
    put s { gdKeydowns = processKeydowns oldKeydowns events }
  gcurrentTicks = gets gdCurrentTicks
  gcurrentTimeDelta = gets gdTimeDelta
  gcurrentKeydowns = gets gdKeydowns
  grender picture = do
    p <- gets gdPlatform
    sf <- gets gdSurfaces
    bg <- gets gdBackgroundColor
    font <- gets gdFont
    liftIO $ wrenchRender p sf font bg picture
    rbm <- gets gdRenderBlockMode
    case rbm of
      RenderAndReturn -> return ()
      RenderAndWait fps -> do
        lastRender <- gets gdLastRender
        newTicks <- liftIO getTicks
        modify (\oldState -> oldState { gdLastRender = newTicks })
        let waitTime = fromSeconds (1/(fromIntegral fps)) - (newTicks `tickDelta` lastRender)
        when (waitTime > 0) (threadDelay waitTime)
  glookupAnim aid = do
    anims <- gets gdAnims
    return (aid `lookup` anims)
  glookupImageRectangle sid = do
    surfaces <- gets gdSurfaces
    return (snd <$> (sid `lookup` surfaces))

processKeydown :: Event -> Keydowns -> Keydowns
processKeydown (Keyboard (KeyboardEvent KeyUp _ keysym)) = S.delete keysym
processKeydown (Keyboard (KeyboardEvent KeyDown _ keysym)) = S.insert keysym
processKeydown _ = id

processKeydowns :: Keydowns -> [Event] -> Keydowns
processKeydowns = foldr processKeydown

runGame :: FilePath -> P.WindowTitle -> WindowSize -> MouseGrabMode -> Maybe Color -> RenderBlockMode -> MonadGameBackend () -> IO ()
runGame mediaDir title size mouseGrab bgColor renderBlockMode action = withPlatform title size mouseGrab $ do
  \platform -> do
    mediaData <- readMediaFiles (P.loadImage platform) (mediaDir </> "images")
    sounds <- readAudioFiles (P.loadAudio platform) (mediaDir </> "sounds")
    ticks <- getTicks
    font <- P.loadFont platform (mediaDir </> "fonts" </> "stdfont.ttf") 15
    let
      gameData = GameData {
          gdSurfaces = mediaData ^. mdSurfaces
        , gdSounds = sounds
        , gdSources = []
        , gdAnims = mediaData ^. mdAnims
        , gdPlatform = platform
        , gdCurrentTicks = ticks
        , gdBackgroundColor = bgColor
        , gdTimeDelta = fromSeconds 0
        , gdKeydowns = mempty
        , gdFont = font
        , gdRenderBlockMode = renderBlockMode
        , gdLastRender = ticks
        }
    r <- getStdGen
    evalStateT (evalRandT (runGameData action) r) gameData
