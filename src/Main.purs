module Main
  ( Tile(..)
  , World
  , bombTick
  , draw
  , evenWallPlacement
  , handleEvent
  , height
  , isWall
  , main
  , movePlayer
  , setTile
  , width
  )
  where


import Data.Grid (Coordinates, Grid, enumerate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst, snd)
import Effect.Random (randomInt)
import Prelude (class Eq, Unit, bind, const, discard, mod, negate, not, otherwise, pure, when, ($), (&&), (+), (-), (/), (/=), (<), (<$>), (<>), (==), (>=), (||))
import Reactor.Graphics.Colors
import Data.Array as Array
import Data.Grid as Grid
import Data.List (List(..), (:), null, filter)
import Effect (Effect)
import Reactor (executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction)

radiusConst :: Int
radiusConst = 3

width :: Int
width = 18

height :: Int
height = 18

timer :: Int
timer = 180

main :: Effect Unit
main = do
  board <- Grid.constructM width height setTile
  let initial = { player: { x:1, y: 1 }, board }
  let reactor = { initial, draw, handleEvent, isPaused: const false }
  runReactor reactor { title: "Bomberman", width, height, widgets: [] }

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


isBox :: Coordinates -> Effect Boolean
isBox { x, y } = do
  coin <- randomInt 0 2
  pure $ (coin /= 0) && (not $ (x < 4 || x >= width - 4) && (y < 4 || y >= height - 4))


evenWallPlacement :: Boolean -> Int -> Boolean
evenWallPlacement true currentIndex = currentIndex `mod` 2 == 0
evenWallPlacement false currentIndex = (currentIndex - 1) `mod` 2 == 0


setTile :: { x :: Int, y :: Int} -> Effect Tile
setTile point = do
  isBoxBool <- isBox point
  if isWall point then 
    pure Wall 
  else if isBoxBool then
    pure Box 
  else
    pure Empty

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
          hum = (fst a) : bombBoom bombsTicked (fst a) radiusConst Nil
          
          customSetTile point = 
            if not null $ filter (_ == point) hum then 
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


bombBoom ∷ Grid Tile → { x ∷ Int , y ∷ Int } → Int → List { x ∷ Int , y ∷ Int } → List { x ∷ Int , y ∷ Int }
bombBoom board {x, y} radius bombsDone =
  let 
    right = go board {x: x + 1, y} radius {xChange:1, yChange:0} bombsDone
    left = go board {x: x - 1, y} radius {xChange: -1,yChange: 0} bombsDone
    up = go board {x, y: y - 1} radius {xChange:0 ,yChange: -1} bombsDone
    down = go board {x, y: y + 1} radius {xChange:0,yChange: 1} bombsDone
  in
    right <> left <> up <> down
  where
    go board tile@{x, y} radius enum@{xChange, yChange} bombsDonee = 
      let aaa = fromMaybe Empty (Grid.index board tile) in
      case aaa of
        Wall -> Nil
        Bomb{} -> if null $ filter (_ == tile) bombsDonee then tile : (bombBoom board tile radiusConst (tile : bombsDonee)) else Nil
        Box -> tile : Nil
        _ -> if not(radius == 0) then 
            tile : (go board {x: x + xChange, y: y + yChange} (radius - 1) enum bombsDonee)
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