{-# LANGUAGE TemplateHaskell #-}
module Main where

import           ClassyPrelude
import           Control.Lens       ((&), (+~), (-~), (^.))
import           Control.Lens.TH
import           Data.List          (iterate, (!!))
import           Linear.V2
import           Linear.Vector      ((^+^))
import           Wrench.Color
import           Wrench.Engine
import           Wrench.KeyMovement
import qualified Wrench.Keysym      as Key
import           Wrench.Platform
import           Wrench.Point
import           Wrench.Time

type TileIndex = V2 Int

data TetrominoType = TTI | TTO | TTT | TTJ | TTL | TTS | TTZ

data TetrominoInstance = TetrominoInstance {
    _tiType          :: TetrominoType
  , _tiPos           :: TileIndex
  , _tiRotationCount :: Int
  }

$(makeLenses ''TetrominoInstance)

rotateTetromino :: Int -> ( [ TileIndex ],TileIndex ) -> [TileIndex]
rotateTetromino c (ts,r) = ( iterate ( map (\p -> perp ( p-r ) + r) ) ts ) !! c

tetrominoToTiles :: TetrominoInstance -> [ TileIndex ]
tetrominoToTiles t = map (^+^ (t ^. tiPos)) $ rotateTetromino (t ^. tiRotationCount) $ case t ^. tiType of
  TTI -> ([(0,2),(1,2),(2,2),(3,2)],(1,2))
  TTO -> ([(0,0),(0,1),(1,0),(1,1)],(0,0))
  TTT -> ([(0,1),(1,1),(2,1),(1,0)],(1,1))
  TTL -> ([(1,0),(1,1),(1,2),(2,2)],(1,1))
  TTJ -> ([(1,0),(1,1),(1,2),(0,2)],(1,1))
  TTS -> ([(0,1),(1,1),(1,0),(2,0)],(1,1))
  TTZ -> ([(0,0),(1,0),(1,1),(2,1)],(1,1))

data PlayingField g = PlayingField {
    _pfTiles            :: TileIndex -> Maybe Color
  , _pfSize             :: TileIndex
  , _pfLastTick         :: TimeTicks
  , _pfRandomGen        :: g
  , _pfCurrentTetromino :: TetrominoInstance
  }

$(makeLenses ''PlayingField)

playingFieldToPicture :: ViewportSize -> PlayingField g -> Picture
playingFieldToPicture = undefined

gameOver :: PlayingField g -> Bool
gameOver p = let currentTetro = tetrominoToTiles ( p ^. pfCurrentTetromino )
                 outside (V2 x y) = x < 0 || y < 0 || x >= (p & pfSize . _x) || y >= (p & pfSize . _y)
             in any isJust ( map (p ^. pfTiles) currentTetro ) || any outside currentTetro

orSafe :: PlayingField g -> PlayingField g -> PlayingField g
orSafe old new = if gameOver new then old else new

eventHandler :: Event -> PlayingField g -> PlayingField g
eventHandler event world = world `orSafe` case event of
  Keyboard{keyMovement=KeyDown, keySym=Key.Left} -> world & pfCurrentTetromino . tiPos +~ V2 (-1) 0
  Keyboard{keyMovement=KeyDown, keySym=Key.Right} -> world & pfCurrentTetromino . tiPos +~ V2 1 0
  Keyboard{keyMovement=KeyDown, keySym=Key.Up} -> world & pfCurrentTetromino . tiRotationCount +~ 1
  Keyboard{keyMovement=KeyDown, keySym=Key.Down} -> world & pfCurrentTetromino . tiRotationCount +~ 3
  _ -> world

toPicture :: ViewportSize -> World -> Picture
toPicture _ world = Translate (carPosition world) $ Sprite "car" RenderPositionCenter

initialWorld :: World
initialWorld = World (V2 100 100)

main :: IO ()
main = do
  withPlatform (WindowTitle "wrenchtris") $ \p -> do
    wrenchPlay
        p
        (MediaPath "media")
        (BackgroundColor (Just colorsBlack))
        initialWorld
        (StepsPerSecond 1)
        (ToPictureHandler toPicture)
        (EventHandler eventHandler)
        (TickHandler (\_ w -> w))
