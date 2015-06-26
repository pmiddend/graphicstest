{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
module Wrench.MonadGame(
    MonadGame(..)
  , runGame
  , glookupAnimUnsafe
  , glookupImageRectangleUnsafe
  ) where

import           Control.Monad.State.Strict (MonadState, StateT, evalStateT,
                                             get, gets, put,modify)
import           Control.Monad.Writer.Lazy (WriterT)
import qualified Data.Set                   as S
import           ClassyPrelude hiding((</>),FilePath)
import Data.Maybe(fromJust)
import           Wrench.Animation
import           Wrench.AnimId
import           Wrench.Color
import           Wrench.Engine
import           Wrench.AudioData
import           Wrench.ImageData
import System.FilePath
import           Wrench.KeyMovement
import           Wrench.PlayMode
import           Wrench.Picture
import           Wrench.Platform            (Platform)
import           Wrench.Event
import qualified Wrench.Platform            as P
import qualified Data.Map.Strict as M
import           Wrench.Rectangle
import           Wrench.Time
import           Wrench.Point
import System.Random(StdGen,getStdGen)
import Control.Monad.Random(MonadRandom(..),RandT,evalRandT)
import Wrench.Keydowns

class MonadGame m where
  gpollEvents :: m [Event]
  gupdateTicks :: Double -> m ()
  gplaySound :: SoundId -> m ()
  gupdateKeydowns :: [Event] -> m ()
  gcurrentTicks :: m TimeTicks
  gcurrentTimeDelta :: m TimeDelta
  gcurrentKeydowns :: m Keydowns
  gviewportSize :: m Point
  grender :: Picture -> m ()
  glookupAnim :: AnimId -> m (Maybe Animation)
  glookupImageRectangle :: ImageId -> m (Maybe Rectangle)

glookupAnimUnsafe :: (Functor m,MonadGame m) => AnimId -> m Animation 
glookupAnimUnsafe anim = fromJust <$> glookupAnim anim

glookupImageRectangleUnsafe :: (Functor m,MonadGame m) => ImageId -> m Rectangle
glookupImageRectangleUnsafe im = fromJust <$> glookupImageRectangle im

data GameData p = GameData {
    gdSurfaces     :: SurfaceMap (P.PlatformImage p)
  , gdSounds       :: SoundMap (P.PlatformAudioBuffer p)
  , gdSources      :: [P.PlatformAudioSource p]
  , gdAnims        :: AnimMap
  , gdPlatform     :: p
  , gdBackgroundColor     :: Maybe Color
  , gdCurrentTicks :: !TimeTicks
  , gdTimeDelta    :: !TimeDelta
  , gdKeydowns     :: !Keydowns
  , gdFont         :: P.PlatformFont p
  }

newtype GameDataM p a = GameDataM {
  runGameData :: RandT StdGen (StateT (GameData p) IO) a
  } deriving(Monad,MonadRandom,MonadIO,MonadState (GameData p),Applicative,Functor)

instance (Monad m,MonadGame m) => MonadGame (StateT n m) where
  gpollEvents = lift gpollEvents
  gplaySound s = lift (gplaySound s)
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

runGame :: FilePath -> P.WindowTitle -> P.WindowSize -> Maybe Color -> GameDataM PlatformBackend () -> IO ()
runGame mediaDir title size bgColor action = withPlatform title size $ do
  \platform -> do
    (images, anims) <- readMediaFiles (P.loadImage platform) (mediaDir </> "images")
    sounds <- readAudioFiles (P.loadAudio platform) (mediaDir </> "sounds")
    ticks <- getTicks
    font <- P.loadFont platform (mediaDir </> "fonts" </> "stdfont.ttf") 15
    let
      gameData = GameData {
          gdSurfaces = images
        , gdSounds = sounds
        , gdSources = []
        , gdAnims = anims
        , gdPlatform = platform
        , gdCurrentTicks = ticks
        , gdBackgroundColor = bgColor
        , gdTimeDelta = fromSeconds 0
        , gdKeydowns = mempty
        , gdFont = font
        }
    r <- getStdGen
    evalStateT (evalRandT (runGameData action) r) gameData
