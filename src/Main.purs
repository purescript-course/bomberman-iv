module Main
  ( MujEnum
  , Tile
  , World
  , draw
  , height
  , initial
  , isWall
  , main
  , reactor
  , width
  )
  where


import Prelude

import Data.Grid (Grid, Coordinates)
import Data.Grid as Grid
import Data.Maybe (Maybe(..))
import Effect (Effect)
import  Data.EuclideanRing

import Reactor (Reactor, executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction)


width :: Int
width = 10

height :: Int
height = 12

main :: Effect Unit
main = runReactor reactor { title: "Bomberman", width, height }

data Tile = Wall | Empty

data MujEnum
  = Up | Down | Right | Left

derive instance tileEq :: Eq Tile

type World = { player :: Coordinates, board :: Grid Tile }

isWall :: Coordinates -> Boolean
isWall { x, y } = 
  let 
    bool1 = (x == 0 || x == (width - 1) || y == 0 || y == (height - 1)) -- borders
    bool2 = if (width  `mod` 2 /= 0) then x `mod` 2 == 0 else --  <- jeto lichý
      let comparison = compare x (width / 2) in 
      case comparison of
      LT -> x `mod` 2 == 0
      _ -> (x - 1) `mod` 2 == 0 
    bool3 = if (height `mod` 2 /= 0) then y `mod` 2 == 0 else --  <- jeto lichý
      let comparison = compare y (height / 2) in 
      case comparison of
      LT -> y `mod` 2 == 0
      _ -> (y - 1) `mod` 2 == 0                                                              
  in
    bool1 || bool2 && bool3

reactor :: Reactor World
reactor = { initial, draw, handleEvent, isPaused: const true }

initial :: World
initial = { player: { x: width / 2, y: height / 2 }, board }
  where
  board = Grid.construct width height (\point -> if isWall point then Wall else Empty)

draw :: World -> Drawing
draw { player, board } = do
  drawGrid board drawTile
  fill Color.blue400 $ tile player
  where
  drawTile Empty = Just Color.green50
  drawTile Wall = Just Color.gray500

handleEvent :: Event -> Reaction World
handleEvent event = do

  let
    movePlayer enum = do
      { player: { x, y }, board } <- getW
      let newPlayerPosition = case enum of
            Left -> { x: x -1, y: y}
            Right -> { x: x + 1, y: y}
            Down -> { x: x, y: y + 1}
            Up -> { x: x, y: y - 1}
      when (isEmpty newPlayerPosition board) $
        updateW_ { player: newPlayerPosition }
      where
      isEmpty position board = Grid.index board position == Just Empty


  case event of
    KeyPress { key: "ArrowLeft" } -> movePlayer Left
    KeyPress { key: "ArrowRight" } -> movePlayer Right
    KeyPress { key: "ArrowDown" } -> movePlayer Down
    KeyPress { key: "ArrowUp" } -> movePlayer Up
    --Tick{} -> movePlayer Up
    _ -> executeDefaultBehavior
