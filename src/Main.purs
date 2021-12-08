module Main
  ( Tile
  , World
  , bombTick
  , draw
  , evenWallPlacement
  , handleEvent
  , height
  , initial
  , isWall
  , main
  , movePlayer
  , reactor
  , setTile
  , width
  )
  where




import Prelude (class Eq, Unit, bind, const, discard, mod, negate, not, otherwise, when, ($), (&&), (+), (-), (/), (/=), (<), (<$>), (==), (>=), (||))
import Reactor.Graphics.Colors (blue400, blue600, gray300, green600, red600, yellow700)
import Data.Grid (Grid, Coordinates)
import Data.Grid as Grid
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Reactor (Reactor, executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction)



width :: Int
width = 18


height :: Int
height = 18

timer :: Int
timer = 3

main :: Effect Unit
main = runReactor reactor { title: "Bomberman", width, height }

data Tile = Wall | Box | Bomb {time :: Int}| Empty

derive instance tileEq :: Eq Tile
type World = { player :: Coordinates, board :: Grid Tile }

isWall :: Coordinates -> Boolean
isWall { x, y } = 
  let 
    isBorder = (x == 0 || x == (width - 1) || y == 0 || y == (height - 1)) -- borders
    
    isHorizontalWall = isInnerWall width x
      
    isVerticalWall = isInnerWall height y                                                       
  in
    isBorder || (isHorizontalWall && isVerticalWall)
  where
    isInnerWall dimension point  
      | dimension `mod` 2 /= 0 = point `mod` 2 == 0 
      | otherwise = evenWallPlacement (point < (dimension / 2)) point


isBox :: Coordinates -> Boolean
isBox { x, y } = not (x < 4 || x >= width - 4) && (y < 4 || y >= height - 4)


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
setTile point  
  | isWall point = Wall 
  | isBox point = Box 
  | otherwise = Empty

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
      let bumbac = bombTick <$> board
      updateW_{board: bumbac}
    KeyPress { key: "ArrowRight" } -> do 
      movePlayer {xDiff: 1, yDiff: 0}
      let bumbac = bombTick <$> board
      updateW_{board: bumbac}
    KeyPress { key: "ArrowDown" } -> do 
      movePlayer {xDiff: 0, yDiff: 1}
      let bumbac = bombTick <$> board
      updateW_{board: bumbac}
    KeyPress { key: "ArrowUp" } -> do 
      movePlayer {xDiff: 0, yDiff: -1}
      let bumbac = bombTick <$> board
      updateW_{board: bumbac}
    KeyPress { key: " " } -> do 
      let placeBomb = Grid.updateAt' {x,y} (Bomb{time: timer}) board 
      let bumbac = bombTick <$> placeBomb
      updateW_{board: bumbac}
    --Tick{} -> movePlayer Up
    _ -> executeDefaultBehavior

bombTick :: Tile -> Tile
bombTick (Bomb {time})
  |time == 0 = Empty
  |otherwise = Bomb {time: time - 1}
bombTick t = t

movePlayer :: {xDiff :: Int, yDiff :: Int} -> Reaction World
movePlayer {xDiff, yDiff} = do
  {player: { x, y }, board } <- getW
  let newPlayerPosition = { x: x + xDiff, y: y + yDiff}
  when (isEmpty newPlayerPosition board) $
    updateW_ { player: newPlayerPosition }
  where
  isEmpty position board = Grid.index board position == Just Empty