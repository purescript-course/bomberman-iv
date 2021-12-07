module Main
  ( Tile
  , World
  , draw
  , evenWallPlacement
  , handleEvent
  , height
  , initial
  , isWall
  , main
  , movePlayer
  , reactor
  , width
  )
  where


import Data.Array
import Debug
import Prelude
import Reactor.Graphics.Colors

import Data.Array (modifyAt)
import Data.Grid (Grid, Coordinates)
import Data.Grid as Grid
import Data.Int.Bits (xor)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Reactor (Reactor, executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction)
import Web.HTML.Event.BeforeUnloadEvent.EventTypes (beforeunload)
import Web.HTML.Event.EventTypes (offline)

width :: Int
width = 18


height :: Int
height = 18

timer :: Int
timer = 3

main :: Effect Unit
main = runReactor reactor { title: "Bomberman", width, height }

data Tile = Wall | Box | Bomb{time :: Int}| Empty

derive instance tileEq :: Eq Tile
type World = { player :: Coordinates, board :: Grid Tile }

isWall :: Coordinates -> Boolean
isWall { x, y } = 
  let 
    isBorder = (x == 0 || x == (width - 1) || y == 0 || y == (height - 1)) -- borders
    
    isHorizontalWall = 
      if (width  `mod` 2 /= 0) then 
        x `mod` 2 == 0 
      else 
        evenWallPlacement (x < (width / 2)) x
      
    isVerticalWall = 
      if (height `mod` 2 /= 0) then 
        y `mod` 2 == 0 
      else 
        evenWallPlacement (y < (height / 2)) y                                                          
  in
    isBorder || (isHorizontalWall && isVerticalWall)

isBox :: Coordinates -> Boolean
isBox { x, y } = let
  leftTopCorn = (x < 3 && y < 3)
  rightTopCorn = (x >= width - 3 && y < 3)
  leftBottomCorn = (x < 3 && y >= height - 3)
  rightBottomCorn = (x >= width - 3  && y >= height - 3)
  in
    not  (leftTopCorn || rightTopCorn || leftBottomCorn || rightBottomCorn)

evenWallPlacement :: Boolean -> Int -> Boolean
evenWallPlacement true currentIndex = currentIndex `mod` 2 == 0
evenWallPlacement _ currentIndex = (currentIndex - 1) `mod` 2 == 0

reactor :: Reactor World
reactor = { initial, draw, handleEvent, isPaused: const true }

initial :: World
initial = { player: { x:1, y: 1 }, board }
  where
  board = Grid.construct width height setTile

setTile :: { x :: Int, y :: Int} -> Tile
setTile point = 
  if isWall point then Wall 
  else if isBox point then Box 
  else Empty

draw :: World -> Drawing
draw { player, board } = do
  drawGrid board drawTile
  fill blue400 $ tile player
  where
  drawTile Empty = Just green600
  drawTile (Bomb{time}) = if time >= (timer / 3) then Just blue600 else Just red600 -- klidně to můžem modulem udělat blikací
  drawTile Wall = Just gray300
  drawTile Box = Just yellow700

handleEvent :: Event -> Reaction World
handleEvent event = do
  {player: { x, y }, board } <- getW
  case event of
    KeyPress { key: "ArrowLeft" } -> do
      movePlayer {xDiff: -1, yDiff: 0}
      tickBombs
    KeyPress { key: "ArrowRight" } -> do 
      movePlayer {xDiff: 1, yDiff: 0}
      tickBombs
    KeyPress { key: "ArrowDown" } -> do 
      movePlayer {xDiff: 0, yDiff: 1}
      tickBombs
    KeyPress { key: "ArrowUp" } -> do 
      movePlayer {xDiff: 0, yDiff: -1}
      tickBombs
    KeyPress { key: " " } -> do 
      let bumbac = Grid.updateAt' {x,y} (Bomb{time: timer - 1}) board  --idk proč -1 ale takhle to tikne právě 3x, což je timer, idk...
      tickBombs
      updateW_ {board: bumbac}
    --Tick{} -> movePlayer Up
    _ -> executeDefaultBehavior


tickBombs = tickBombss 0 0
tickBombss x y = do
  --scrollování řádkama
  let yy = if x == width - 1 then y + 1 else y
  let xx = if x == width - 1 then 0 else x

  { board } <- getW
  case Grid.index board {x: xx, y:yy} of
    Nothing -> executeDefaultBehavior -- vlastně default case v rekurzi, ono to vrátí nothing když jsem out of bounds
    Just a -> case a of
      Bomb{time} -> do 
        let sakumprask = if time == 0 then Grid.updateAt' {x: xx,y: yy} Empty board else Grid.updateAt' {x: xx,y: yy} (Bomb{time: time - 1}) board
        updateW_ {board: sakumprask}
        tickBombss (xx + 1) yy
      _ -> tickBombss (xx + 1) yy -- checkne celý grid
movePlayer :: {xDiff :: Int, yDiff :: Int} -> Reaction World
movePlayer {xDiff, yDiff} = do
  {player: { x, y }, board } <- getW
  let newPlayerPosition = { x: x + xDiff, y: y + yDiff}
  when (isEmpty newPlayerPosition board) $
    updateW_ { player: newPlayerPosition }
  where
  isEmpty position board = Grid.index board position == Just Empty

-- movePlayer direction = do
--   { player: { x, y }, board } <- getW
--   let newPlayerPosition = case direction of
--         Left -> { x: x -1, y: y}
--         Right -> { x: x + 1, y: y}
--         Down -> { x: x, y: y + 1}
--         Up -> { x: x, y: y - 1}
--   when (isEmpty newPlayerPosition board) $
--     updateW_ { player: newPlayerPosition }
--   where
--   isEmpty position board = Grid.index board position == Just Empty

