module Main where

import Prelude
import Data.Array (replicate)
import Data.Grid (Coordinates, Grid, construct)

import Effect (Effect)
import Reactor (Reactor, dimensions, executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, fill, tile)

import Reactor.Reaction (Reaction)


data Cell = Wall | Empty

type World = {
  board :: Grid Cell,
  items :: Array (Array Cell),
  player :: Coordinates
}

data MujEnum
  = Up | Down | Right | Left

width :: Int
width = 20
height :: Int
height = 20

main :: Effect Unit
main = runReactor reactor { title: "Moving Dot", width, height}

reactor :: Reactor World
reactor = { initial, draw, handleEvent, isPaused: const true }

initial :: World
initial = let 
  board = construct width height (\cords -> Empty)
  items = replicate height $ replicate width Empty in 
  {board, items, player: { x: 0, y: 0 }}

draw :: World -> Drawing
draw { player } = do
  fill Color.blue400 $ tile player

handleEvent :: Event -> Reaction World
handleEvent event = 
  do
    { width, height } <- dimensions
    let
      clip a m = min (max 0 a) (m - 1)
      bound { x, y } = { x: clip x width, y: clip y height } 
    { player: { x, y } } <- getW
    let
      mojeFunkce mujEnum = 
        case mujEnum of
          Left -> updateW_ { player: bound { x: x - 1, y } }
          Right -> updateW_ { player: bound { x: x + 1, y } }
          Down -> updateW_ { player: bound { x, y: y + 1 } }
          Up -> updateW_ { player: bound { x, y: y - 1 } }
    case event of
      KeyPress { key: "ArrowLeft" } -> mojeFunkce Left
      KeyPress { key: "ArrowRight" } -> mojeFunkce Right
      KeyPress { key: "ArrowDown" } -> mojeFunkce Down
      KeyPress { key: "ArrowUp" } -> mojeFunkce Up
      _ -> executeDefaultBehavior
        
      
    



