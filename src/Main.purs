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




import Data.Array
import Data.Grid
import Data.List
import Data.Maybe
import Data.Tuple

import Effect.Random
import Prelude
import Prelude
import Reactor.Graphics.Colors
import Data.Array as Array
import Color.Scheme.X11 (turquoise)
import Data.Enum (downFrom)
import Data.Grid as Grid
import Data.HeytingAlgebra.Generic (genericDisj)
import Data.List as List
import Data.List ((:), null)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.HTML (elementNS)
import Reactor (Reactor, executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction, ReactionM(..))
import Web.HTML.Event.EventTypes (offline)

radiusConst :: Int
radiusConst = 3

width :: Int
width = 18

height :: Int
height = 18

timer :: Int
timer = 180

main :: Effect Unit
main = runReactor reactor { title: "Bomberman", width, height }

data Tile = Wall | Box | Bomb {time :: Int}| Explosion{existTime :: Int} | Empty

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
isBox { x, y } = not $ (x < 4 || x >= width - 4) && (y < 4 || y >= height - 4)


evenWallPlacement :: Boolean -> Int -> Boolean
evenWallPlacement true currentIndex = currentIndex `mod` 2 == 0
evenWallPlacement false currentIndex = (currentIndex - 1) `mod` 2 == 0

reactor :: Reactor World
reactor = { initial, draw, handleEvent, isPaused: const false }

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
  drawTile (Explosion{}) = Just blue800
  drawTile Empty = Just green600
  drawTile (Bomb{time}) = if time - 1 >= timer / 3 then Just gray600 else Just red600 -- use mod for flickering
  drawTile Wall = Just gray300
  drawTile Box = Just yellow700

handleEvent :: Event -> Reaction World
handleEvent event = do
  {player: { x, y }, board } <- getW
  
  case event of
    KeyPress { key: "ArrowLeft" } -> do
      movePlayer {xDiff: -1, yDiff: 0}
    KeyPress { key: "ArrowRight" } -> do 
      movePlayer {xDiff: 1, yDiff: 0}
    KeyPress { key: "ArrowDown" } -> do 
      movePlayer {xDiff: 0, yDiff: 1}
    KeyPress { key: "ArrowUp" } -> do 
      movePlayer {xDiff: 0, yDiff: -1}
    KeyPress { key: " " } -> do 
      let bombPlanted = Grid.updateAt' {x,y} (Bomb{time: timer}) board 
      updateW_{board: bombPlanted}
    Tick {} -> 
      let
        bombsTicked = bombTick <$> board
        
        fun a = let 
          hum = (fst a) : bombBoom bombsTicked (fst a) radiusConst
          
          customSetTile point = 
            if not null $ List.filter (_ == point) hum then 
              Explosion {existTime: 120} 
            else fromMaybe Empty $ Grid.index bombsTicked point
        
        in 
          Grid.construct width height customSetTile
      in
        do
          updateW_ {board: bombsTicked}
         
          let funk a = case snd a of 
                Bomb{time} -> time == 1
                _ -> false
          
          let listTiluuSBombou = Array.filter funk $ enumerate bombsTicked 
          updateW_{board: fromMaybe bombsTicked $ Array.head $ fun <$> listTiluuSBombou}

    _ -> executeDefaultBehavior


bombBoom ∷ Grid Tile → { x ∷ Int , y ∷ Int } → Int → List { x ∷ Int , y ∷ Int }
bombBoom board {x, y} radius =
  let 
    right = go board {x: x + 1, y} radius {xChange:1, yChange:0}
    left = go board {x: x - 1, y} radius {xChange: -1,yChange: 0}
    up = go board {x, y: y - 1} radius {xChange:0 ,yChange: -1}
    down = go board {x, y: y + 1} radius {xChange:0,yChange: 1}
  in
    right <> left <> up <> down
  where
    go board tile@{x, y} radius enum@{xChange, yChange} = 
      let aaa = fromMaybe Empty (Grid.index board tile) in
      case aaa of
        Wall -> Nil
        Bomb{} -> tile : Nil
        Box -> tile : Nil
        _ -> if not(radius == 0) then 
            Cons tile (go board {x: x + xChange, y: y + yChange} (radius - 1) enum)
            else Nil

bombTick :: Tile -> Tile
bombTick (Bomb {time}) = Bomb {time: time - 1}
bombTick (Explosion{existTime}) = if existTime < 0 then Empty else Explosion{existTime: existTime - 1}
bombTick tile = tile

movePlayer :: {xDiff :: Int, yDiff :: Int} -> Reaction World
movePlayer {xDiff, yDiff} = do
  {player: { x, y }, board } <- getW
  let newPlayerPosition = { x: x + xDiff, y: y + yDiff}
  when (isEmpty newPlayerPosition board) $
    updateW_ { player: newPlayerPosition }
  where
  isEmpty position board = Grid.index board position == Just Empty