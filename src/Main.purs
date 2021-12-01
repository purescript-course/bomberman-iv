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


import Prelude (class Eq, Unit, bind, const, discard, mod, negate, not, when, ($), (&&), (+), (-), (/), (/=), (<), (==), (>=), (||))

import Data.Grid (Grid, Coordinates)
import Data.Grid as Grid
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Reactor (Reactor, executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors (blue400, gray300, green600, yellow700)
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction)


width :: Int
width = 42

height :: Int
height = 18

main :: Effect Unit
main = runReactor reactor { title: "Bomberman", width, height }

data Tile = Wall |Box| Empty

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
setTile point = if isWall point then Wall else if isBox point then Box else Empty

draw :: World -> Drawing
draw { player, board } = do
  drawGrid board drawTile
  fill blue400 $ tile player
  where
  drawTile Empty = Just green600
  drawTile Wall = Just gray300
  drawTile Box = Just yellow700

handleEvent :: Event -> Reaction World
handleEvent event = do
  case event of
    KeyPress { key: "ArrowLeft" } -> movePlayer {xDiff: -1, yDiff: 0}
    KeyPress { key: "ArrowRight" } -> movePlayer {xDiff: 1, yDiff: 0}
    KeyPress { key: "ArrowDown" } -> movePlayer {xDiff: 0, yDiff: 1}
    KeyPress { key: "ArrowUp" } -> movePlayer {xDiff: 0, yDiff: -1}
    --Tick{} -> movePlayer Up
    _ -> executeDefaultBehavior

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

