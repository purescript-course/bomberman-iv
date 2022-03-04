module Main where

import Prelude
import Data.Array ((!!), length, index)
import Data.Array as Array
import Data.Grid (Grid, Coordinates, enumerate)
import Data.Grid as Grid
import Data.Int (toNumber)
import Data.Int (toStringAs, decimal)
import Data.List (List(..), (:), null, filter, concat, fromFoldable, find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Math (sqrt, pow)
import Reactor (executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors (blue400, gray300, gray600, gray500, hsl, red600, green700)
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Internal.Widget (Widget(..))
import Reactor.Reaction (Reaction, ReactionM, widget)
import Web.HTML.Event.EventTypes (offline)

radiusConst :: Int
radiusConst = 5

width :: Int
width = 18

height :: Int
height = 18

timer :: Int
timer = 200

walkingSpeed ∷ Int
walkingSpeed = 35

runningSpeed :: Int
runningSpeed = walkingSpeed / 3 * 2

main :: Effect Unit
main = do
  board <- Grid.constructM width height setTile
  let
    reactor = reactorF board
  runReactor reactor { title: "Bomberman", width, height, widgets: [] }

initialF board = { player: { cords: { x: 1, y: 1 }, hp: 100 }, player2: { cords: { x: width - 2, y: height - 2 }, isOnRun: { running: false, time: 0 }, lastSeen: { x: width - 2, y: height - 2 } }, board, tickCounter: 0 }

reactorF board = { initial: initialF board, draw, handleEvent, isPaused: const false }

restart ∷ Effect Unit
restart = do
  board <- Grid.constructM width height setTile
  let
    reactor = reactorF board
  runReactor reactor { title: "Bomberman", width, height, widgets: [] }

data Tile
  = Wall
  | Box
  | Bomb { time :: Int }
  | Explosion { existTime :: Int, distance :: Int }
  | Empty

type Enemy
  = { cords :: Coordinates, isOnRun :: Timer, lastSeen :: Coordinates }

type Timer
  = { running :: Boolean, time :: Int }

derive instance tileEq :: Eq Tile

type World
  = { player :: { cords :: Coordinates, hp :: Int }, player2 :: Enemy, board :: Grid Tile, tickCounter :: Int }

directions ∷ Array { xDiff ∷ Int, yDiff ∷ Int }
directions = [ { xDiff: -1, yDiff: 0 }, { xDiff: 1, yDiff: 0 }, { xDiff: 0, yDiff: 1 }, { xDiff: 0, yDiff: -1 } ]

isWall :: Coordinates -> Boolean
isWall { x, y } =
  let
    isBorder = x == 0 || x == (width - 1) || y == 0 || y == (height - 1) -- borders

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

setTile :: { x :: Int, y :: Int } -> Effect Tile
setTile point = do
  isBoxBool <- isBox point
  let
    isWallBool = isWall point
  evaluateBools isBoxBool isWallBool
  where
  evaluateBools isBoxBool isWallBool
    | isWallBool = pure Wall
    | isBoxBool = pure Box
    | otherwise = pure Empty

draw :: World -> Drawing
draw { player: { cords: cords1 }, player2: { cords }, board } = do
  drawGrid board drawTile
  fill blue400 $ tile cords1
  fill red600 $ tile cords
  where
  drawTile (Explosion { distance }) = Just (hslVal distance)

  drawTile Empty = Just gray300

  drawTile (Bomb { time }) = if time - 1 >= timer / 3 || (time - 1) `mod` 10 > 4 then Just gray500 else Just red600

  drawTile Wall = Just gray600

  drawTile Box = Just green700

  hslVal a =
    let
      b = toNumber a

      exRadiusNum = toNumber radiusConst

      division = 4.6416 / exRadiusNum

      hue = 60.0 - sqrt (60.0 * 60.0 / exRadiusNum * b)

      saturation = (100.0 - ((division * b) * pow (division * b) 2.0)) / 100.0

      light = (50.0 - sqrt (18.0 * 18.0 / exRadiusNum * b)) / 100.0
    in
      hsl hue saturation light

handleEvent :: Event -> Reaction World
handleEvent event = do
  { player: player@{ cords: { x, y }, hp }, board, player2: player2@{ cords, isOnRun: { running, time } }, tickCounter } <- getW
  case event of
    KeyPress { key: "ArrowLeft" } -> do
      movePlayer { xDiff: -1, yDiff: 0 }
    KeyPress { key: "ArrowRight" } -> do
      movePlayer { xDiff: 1, yDiff: 0 }
    KeyPress { key: "ArrowDown" } -> do
      movePlayer { xDiff: 0, yDiff: 1 }
    KeyPress { key: "ArrowUp" } -> do
      movePlayer { xDiff: 0, yDiff: -1 }
    KeyPress { key: " " } -> do
      let
        bombPlanted = Grid.updateAt' { x, y } (Bomb { time: timer }) board
      updateW_ { board: bombPlanted }
    Tick {} -> do
      coin <- liftEffect $ randomInt 0 999999 --hehe; abychom mohli použít jeden coin na více věcí, dělám to takhle.
      let
        isOnRun = { running: (time > 0), time: time - 1 }
      updateW_ { board: bombTick <$> board, tickCounter: tickCounter + 1, player2: player2 { isOnRun = isOnRun } }
      when (coin `mod` 5 < 4 && tickCounter `mod` (if running then runningSpeed else walkingSpeed) == 0) movePlayer2 --p = 0,8
      if coin `mod` 100 < 69 && tickCounter `mod` 300 == 0 then --p = 0,7
        let
          bombPlanted = Grid.updateAt' cords (Bomb { time: timer }) board

          newPlayer2 = player2 { isOnRun = { running: true, time: radiusConst * runningSpeed } }
        in
          updateW_ { board: bombPlanted, player2: newPlayer2 }
      else
        pure unit
      { board: newBoard } <- getW --just to be sure we are working with the most recent board
      let
        explodingBombs = filter isGonnaExplode $ (fromFoldable <<< enumerate) newBoard
      let
        explodingCords = concat $ map (\bomb -> explode newBoard (fst bomb)) explodingBombs
      let
        customSetTile tileCords = case find (\boomCords -> snd boomCords == tileCords) explodingCords of
          Nothing -> fromMaybe Empty $ Grid.index newBoard tileCords
          Just a -> Explosion { existTime: 120, distance: fst a } --fst a is distance from bomb
      let
        newWorld = Grid.construct width height customSetTile
      updateW_ { board: newWorld }
      when (isExplosion { x, y } newWorld && tickCounter `mod` 2 == 0) $ updateW_ { player: player { hp = hp - 1 } }
      widget "healthTitle" (Section { title: "Health:" })
      widget "HP" (Label { content: toStringAs decimal hp })
    _ -> executeDefaultBehavior

isExplosion :: { x :: Int, y :: Int } -> Grid Tile -> Boolean
isExplosion position board = case Grid.index board position of
  Just (Explosion {}) -> true
  _ -> false

isGonnaExplode :: Tuple { x ∷ Int, y ∷ Int } Tile -> Boolean
isGonnaExplode tile = case snd tile of
  Bomb { time } -> time == 1
  _ -> false

explode :: Grid Tile -> { x :: Int, y :: Int } -> List (Tuple Int { x :: Int, y :: Int })
explode board { x, y } = bombBoom board { x, y } radiusConst Nil

bombBoom ∷ Grid Tile → { x ∷ Int, y ∷ Int } → Int → List { x ∷ Int, y ∷ Int } → List (Tuple Int { x ∷ Int, y ∷ Int })
bombBoom board { x, y } radius bombsDone =
  let
    right = go board { x: x + 1, y } radius { xChange: 1, yChange: 0 } bombsDone

    left = go board { x: x - 1, y } radius { xChange: -1, yChange: 0 } bombsDone

    up = go board { x, y: y - 1 } radius { xChange: 0, yChange: -1 } bombsDone

    down = go board { x, y: y + 1 } radius { xChange: 0, yChange: 1 } bombsDone

    go board_ tile@{ x: expX, y: expY } radius_ enum@{ xChange, yChange } bombsDonee = case fromMaybe Empty $ Grid.index board_ tile of
      Wall -> Nil
      Bomb {} -> if null $ filter (_ == tile) bombsDonee then (0 /\ tile) : bombBoom board_ tile radiusConst (tile : bombsDonee) else Nil
      Box -> (distanceFromCords tile /\ tile) : Nil
      _ ->
        if not (radius_ == 0) then
          (distanceFromCords tile /\ tile) : go board_ { x: expX + xChange, y: expY + yChange } (radius_ - 1) enum bombsDonee
        else
          Nil

    distanceFromCords a = absoluteValue (a.x - x) + absoluteValue (a.y - y)

    absoluteValue i = if i < 0 then i * -1 else i
  in
    (Tuple 0 { x, y }) {- bomb cords -} : right <> left <> up <> down

bombTick :: Tile -> Tile
bombTick (Bomb { time }) = Bomb { time: time - 1 }

bombTick (Explosion ex@{ existTime }) = if existTime < 0 then Empty else Explosion ex { existTime = existTime - 1 }

bombTick tile = tile

movePlayer :: { xDiff :: Int, yDiff :: Int } -> Reaction World
movePlayer { xDiff, yDiff } = do
  { player: player@{ cords: { x, y } }, board } <- getW
  let
    newPlayerPosition = { x: x + xDiff, y: y + yDiff }
  when (possiblePos newPlayerPosition board) $ updateW_ { player: player { cords = newPlayerPosition } }

possiblePos :: { x :: Int, y :: Int } -> Grid Tile -> Boolean
possiblePos position board = isEmpty position board || isExplosion position board

movePlayer2 :: Reaction World
movePlayer2 = do
  running /\ { xDiff, yDiff } <- enemyDirection
  { player2: { cords: cords@{ x, y }, isOnRun: isOnRun@{ time } } } <- getW
  let
    newPlayerPosition = { x: x + xDiff, y: y + yDiff }
  updateW_ { player2: { cords: newPlayerPosition, isOnRun: isOnRun { time = if not running then 0 else time }, lastSeen: cords } }

enemyDirection ∷ ReactionM World (Tuple Boolean { xDiff ∷ Int, yDiff ∷ Int })
enemyDirection = do
  { board, player2: { cords: cords@{ x: x1, y: y1 }, isOnRun: { running }, lastSeen } } <- getW
  coin <- liftEffect $ randomInt 0 999999
  let
    possibleDirections = Array.filter (wayIsOK cords lastSeen board) directions
  let
    possibleTurns = Array.filter (not isATurn cords lastSeen) possibleDirections
  pure
    $ if isEmpty lastSeen board && (Array.null possibleDirections || (isBomb $ fromMaybe Empty $ Grid.index board cords)) then do
        running /\ { xDiff: lastSeen.x - x1, yDiff: lastSeen.y - y1 }
      else if running && not Array.null possibleTurns then
        false /\ (fromMaybe { xDiff: 0, yDiff: 0 } <<< index possibleTurns) (coin `mod` length possibleTurns)
      else
        running /\ (fromMaybe { xDiff: 0, yDiff: 0 } <<< index possibleDirections) (coin `mod` length possibleDirections)

isATurn ∷ { x ∷ Int, y ∷ Int } → { x ∷ Int, y ∷ Int } → { xDiff ∷ Int, yDiff ∷ Int } → Boolean
isATurn { x: currX, y: currY } { x: prevX, y: prevY } { xDiff, yDiff } = { x: currX + xDiff, y: currY + yDiff } == { x: prevX, y: prevY } || { x: currX + (xDiff * -1), y: currY + (yDiff * -1) } == { x: prevX, y: prevY }

isEmpty :: { x :: Int, y :: Int } -> Grid Tile -> Boolean
isEmpty position board = Grid.index board position == Just Empty

isBomb :: Tile -> Boolean
isBomb tile = case tile of
  Bomb {} -> true
  _ -> false

wayIsOK ∷ { x ∷ Int, y ∷ Int } → { x ∷ Int, y ∷ Int } → Grid Tile → { xDiff ∷ Int, yDiff ∷ Int } → Boolean
wayIsOK { x, y } lastSeen board { xDiff, yDiff } =
  if { x: x + xDiff, y: y + yDiff } == lastSeen then
    false
  else
    isEmpty { x: x + xDiff, y: y + yDiff } board
