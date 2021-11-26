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


import Reactor (Reactor, executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction)


width :: Int
width = 12

height :: Int
height = 11

main :: Effect Unit
main = runReactor reactor { title: "Bomberman", width, height }

data Tile = Wall | Empty

data MujEnum
  = Up | Down | Right | Left

derive instance tileEq :: Eq Tile

type World = { player :: Coordinates, board :: Grid Tile }

isWall :: Coordinates -> Boolean
isWall { x, y } = x == 0 || x == (width - 1) || y == 0 || y == (height - 1)

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
