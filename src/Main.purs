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


import Prelude
import Reactor.Graphics.Colors (blue400, gray300, gray600, green600, hsl, red600, yellow800)

import Data.Array as Array
import Data.Grid (Coordinates, Grid, enumerate)
import Data.Grid as Grid
import Data.Int (toNumber)
import Data.List (List(..), (:), null, filter, head)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Random (randomInt)
import Math (sqrt, pow)
import Reactor (executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Internal.Widget (Widget(..))
import Reactor.Reaction (Reaction)

radiusConst :: Int
radiusConst = 5

width :: Int
width = 18

height :: Int
height = 18

timer :: Int
timer = 240

main :: Effect Unit
main = do
  board <- Grid.constructM width height setTile
  let initial = { player: { x:1, y: 1 }, board }
  let reactor = { initial, draw, handleEvent, isPaused: const false }
  runReactor reactor { title: "Bomberman", width, height , widgets: [Tuple "Sexy widget" (Section { title: "Sexy" })]}

data Tile = Wall | Box | Bomb {time :: Int}| Explosion{existTime :: Int, distance :: Int} | Empty

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
  coin <- randomInt 0 4
  pure $ coin /= 0 && (not $ (x < 4 || x >= width - 4) && (y < 4 || y >= height - 4))


evenWallPlacement :: Boolean -> Int -> Boolean
evenWallPlacement true currentIndex = currentIndex `mod` 2 == 0
evenWallPlacement false currentIndex = (currentIndex - 1) `mod` 2 == 0


setTile :: { x :: Int, y :: Int} -> Effect Tile
setTile point = do
  isBoxBool <- isBox point
  let isWallBool = isWall point
  evaluateBools isBoxBool isWallBool
  where
    evaluateBools isBoxBool isWallBool
      |isWallBool = pure Wall 
      |isBoxBool = pure Box 
      |otherwise = pure Empty

draw :: World -> Drawing
draw { player, board } = do
  drawGrid board drawTile
  fill blue400 $ tile player
  where
  drawTile (Explosion{distance}) = Just (hslVal distance)
  drawTile Empty = Just green600
  drawTile (Bomb{time}) = if time - 1 >= timer / 3 ||  (time - 1) `mod` 10 > 4 then Just gray600 else Just red600 -- use mod for flickering
  drawTile Wall = Just gray300
  drawTile Box = Just yellow800
  hslVal a = 
          let 
            b = toNumber a
            exRadiusNum = toNumber radiusConst
            division = 4.6416 / exRadiusNum
            hue = (60.0 - sqrt (60.0 * 60.0 / exRadiusNum  * b))
            saturation = (100.0 - ((division * b) * pow (division * b) 2.0))/100.0
            light = (50.0 - sqrt (18.0 * 18.0 / exRadiusNum  * b))/100.0 in

            hsl hue saturation light
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
          hum = bombBoom bombsTicked (fst a) radiusConst Nil
          
          customSetTile point = 
            let explosionVal = filter (\q -> snd q == point) hum 
                currExp = fromMaybe (Tuple 0 {x:0, y:0}) (head explosionVal)
            in
            if not null explosionVal then 
              Explosion {existTime: 120, distance: fst currExp} 
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


bombBoom ∷ Grid Tile → { x ∷ Int , y ∷ Int } → Int → List { x ∷ Int , y ∷ Int } → List (Tuple Int { x ∷ Int , y ∷ Int } )
bombBoom board {x, y} radius bombsDone =
  let 
    right = go board {x: x + 1, y} radius {xChange:1, yChange:0} bombsDone
    left = go board {x: x - 1, y} radius {xChange: -1,yChange: 0} bombsDone
    up = go board {x, y: y - 1} radius {xChange:0 ,yChange: -1} bombsDone
    down = go board {x, y: y + 1} radius {xChange:0,yChange: 1} bombsDone
  in
    (Tuple 0 {x, y}) : right <> left <> up <> down
  where
    go board_ tile@{x: expX, y: expY} radius_ enum@{xChange, yChange} bombsDonee = 
      let aaa = fromMaybe Empty (Grid.index board_ tile) in
      case aaa of
        Wall -> Nil
        Bomb{} -> if null $ filter (_ == tile) bombsDonee then (Tuple 0 tile) : (bombBoom board_ tile radiusConst (tile : bombsDonee)) else Nil
        Box -> (Tuple (distanceFromCords tile) tile) : Nil
        _ -> if not(radius_ == 0) then 
            (Tuple (distanceFromCords tile) tile) : (go board_ {x: expX + xChange, y: expY + yChange} (radius_ - 1) enum bombsDonee)
            else Nil
    distanceFromCords a = absoluteValue (a.x - x) + absoluteValue (a.y - y)
    absoluteValue i = if i < 0 then i * -1 else i


bombTick :: Tile -> Tile
bombTick (Bomb {time}) = Bomb {time: time - 1}
bombTick (Explosion{existTime, distance}) = if existTime < 0 then Empty else Explosion{existTime: existTime - 1, distance}
bombTick tile = tile

movePlayer :: {xDiff :: Int, yDiff :: Int} -> Reaction World
movePlayer {xDiff, yDiff} = do
  {player: { x, y }, board } <- getW
  let newPlayerPosition = { x: x + xDiff, y: y + yDiff}
  when (isEmpty newPlayerPosition board) $
    updateW_ { player: newPlayerPosition }
  where
  isEmpty position board = Grid.index board position == Just Empty