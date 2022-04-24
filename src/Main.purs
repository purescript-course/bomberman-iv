module Main where

import Prelude
import Data.Array (index, length, (..), zip, updateAt)
import Data.Array as Array
import Data.Foldable (for_)
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
import Reactor.Graphics.Colors (hsl)
import Reactor.Graphics.Drawing (Drawing, drawGridWithIndex, fill, tile)
import Reactor.Internal.Widget (Widget(..))
import Reactor.Reaction (ReactionM, Reaction, widget)

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
  = { player :: { cords :: Coordinates, hp :: Int }, player2 :: Array Enemy, board :: Grid Tile, tickCounter :: Int, mousePos :: Coordinates }

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

lightRange :: Int
lightRange = 7

enemies ∷ Array Enemy
enemies =
  [ { cords: { x: width - 2, y: height - 2 }, isOnRun: { running: false, time: 0 }, lastSeen: { x: width - 2, y: height - 2 } }
  , { cords: { x: width - 2, y: 1 }, isOnRun: { running: false, time: 0 }, lastSeen: { x: width - 2, y: 1 } }
  , { cords: { x: 1, y: height - 2 }, isOnRun: { running: false, time: 0 }, lastSeen: { x: 1, y: height - 2 } }
  ]

directions ∷ Array { xDiff ∷ Int, yDiff ∷ Int }
directions = [ { xDiff: -1, yDiff: 0 }, { xDiff: 1, yDiff: 0 }, { xDiff: 0, yDiff: 1 }, { xDiff: 0, yDiff: -1 } ]

restart ∷ Effect Unit
restart = do
  board <- Grid.constructM width height setTile
  let
    reactor = reactorF board
  runReactor reactor { title: "Bomberman", width, height, widgets: [] }

initialF board = { player: { cords: { x: 1, y: 1 }, hp: 100 }, player2: enemies, board, tickCounter: 0, mousePos: { x: 0, y: 0 } }

reactorF board = { initial: initialF board, draw, handleEvent, isPaused: const false }

main :: Effect Unit
main = do
  board <- Grid.constructM width height setTile
  let
    reactor = reactorF board
  runReactor reactor { title: "Bomberman", width, height, widgets: [] }

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
  coin <- randomInt 0 2
  pure $ coin < 1 && (not $ (x < 4 || x >= width - 4) && (y < 4 || y >= height - 4))

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
draw { player: { cords: cords1 }, player2, board, mousePos } = do
  drawGridWithIndex board drawTile
  fill (hsl 206.82 0.8995 (0.6096 * (getLightValue cords1 mousePos))) $ tile cords1
  for_ player2 (\enemy -> do fill (hsl 1.37 0.7719 (0.5529 * getLightValue enemy.cords mousePos)) $ tile enemy.cords)
  pure unit
  where
  drawTile tileCoords (Explosion { distance }) = Just (hslVal distance tileCoords)

  drawTile tileCoords Empty = Just $ hsl 220.0 0.143 (0.959 * (getLightValue tileCoords mousePos))

  drawTile tileCoords (Bomb { time }) =
    if time - 1 >= timer / 3 || (time - 1) `mod` 10 > 4 then
      Just $ hsl 0.0 0.286 (0.6569 * (getLightValue tileCoords mousePos))
    else
      Just $ hsl 1.37 0.7719 (0.5529 * (getLightValue tileCoords mousePos)) -- flickering

  drawTile tileCoords Wall = Just $ hsl 0.0 0.0 (0.4588 * (getLightValue tileCoords mousePos))

  drawTile tileCoords Box = Just $ hsl 122.79 0.4343 (0.3882 * (getLightValue tileCoords mousePos))

  hslVal distance tileCoords =
    let
      distNum = toNumber distance

      exRadiusNum = toNumber radiusConst

      division = 4.6416 / exRadiusNum

      hue = 60.0 - sqrt (60.0 * 60.0 / exRadiusNum * distNum)

      saturation = (100.0 - ((division * distNum) * pow (division * distNum) 2.0)) / 100.0

      light = ((50.0 - sqrt (18.0 * 18.0 / exRadiusNum * distNum)) / 100.0) * (getLightValue tileCoords mousePos)
    in
      hsl hue saturation light

getLightValue :: Coordinates -> Coordinates -> Number
getLightValue { x, y } { x: mX, y: mY } = toNumber (lightRange * lightRange - ((x - mX) * (x - mX) + (y - mY) * (y - mY))) / toNumber (lightRange * lightRange)

handleP2Events ∷ Tuple Enemy Int -> Reaction World
handleP2Events (player2@{ cords, isOnRun: { running, time } } /\ i) = do
  { board, tickCounter, player2: player2arr } <- getW
  coin <- liftEffect $ randomInt 0 999999
  let
    isOnRun = { running: (time > 0), time: time - 1 }

    arr = fromMaybe [] $ updateAt i (player2 { isOnRun = isOnRun }) player2arr

    moveDelay = (if running then runningSpeed else walkingSpeed) + i

    bombDelay = 199 + i * 50
  updateW_ { player2: arr }
  when (coin `mod` 5 < 4 && tickCounter `mod` moveDelay == 0) $ movePlayer2 (player2 /\ i) --p = 0,8
  when (coin `mod` 100 < 69 && tickCounter `mod` bombDelay == 0) --p = 0,7
    $ let
        bombPlanted = Grid.updateAt' cords (Bomb { time: timer }) board

        runningPlayer2 = player2 { isOnRun = { running: true, time: radiusConst * runningSpeed } }

        newP2Arr = fromMaybe [] $ updateAt i runningPlayer2 player2arr
      in
        updateW_ { board: bombPlanted, player2: newP2Arr }

handleEvent :: Event -> Reaction World
handleEvent event = do
  { player: player@{ cords: { x, y }, hp }, board, player2, tickCounter } <- getW
  case event of
    KeyPress { key: "ArrowLeft" } -> do
      movePlayer { xDiff: -1, yDiff: 0 }
    KeyPress { key: "ArrowRight" } -> do
      movePlayer { xDiff: 1, yDiff: 0 }
    KeyPress { key: "ArrowDown" } -> do
      movePlayer { xDiff: 0, yDiff: 1 }
    KeyPress { key: "ArrowUp" } -> do
      movePlayer { xDiff: 0, yDiff: -1 }
    KeyPress { key: " " } -> updateW_ { board: Grid.updateAt' { x, y } (Bomb { time: timer }) board }
    Mouse { position: { x: mX, y: mY } } -> if (tickCounter `mod` 4) == 0 then updateW_ { mousePos: { x: mX, y: mY } } else executeDefaultBehavior
    Tick {} -> do
      updateW_ { board: bombTick <$> board, tickCounter: tickCounter + 1 } -- Bombs and explosions tick
      for_ (player2 `zip` (0 .. length player2)) handleP2Events --Enemies move and plant bombs
      { board: newBoard } <- getW --just to be sure we are working with the most recent board
      let
        explodingBombs = filter isGonnaExplode $ (fromFoldable <<< enumerate) newBoard

        explodingCords = concat $ map (\bomb -> explode newBoard (fst bomb)) explodingBombs

        customSetTile tileCords = case find (\boomCords -> snd boomCords == tileCords) explodingCords of
          Nothing -> fromMaybe Empty $ Grid.index newBoard tileCords
          Just a -> Explosion { existTime: 120, distance: fst a } --'fst a' is distance from bomb

        newWorld = Grid.construct width height customSetTile
      updateW_ { board: newWorld } -- all bombs exploded
      when (isExplosion { x, y } newWorld && tickCounter `mod` 2 == 0) $ updateW_ { player: player { hp = hp - 1 } }
      widget "healthTitle" (Section { title: "Health:" })
      widget "HP" (Label { content: toStringAs decimal hp })
      when (hp < 1) do
        restartBoard <- liftEffect $ Grid.constructM width height setTile
        updateW_ (initialF restartBoard)
    _ -> executeDefaultBehavior

explode :: Grid Tile -> { x :: Int, y :: Int } -> List (Tuple Int { x :: Int, y :: Int })
--Helper function for bombBoom
explode board { x, y } = bombBoom board { x, y } radiusConst Nil

bombBoom ∷ Grid Tile → { x ∷ Int, y ∷ Int } → Int → List { x ∷ Int, y ∷ Int } → List (Tuple Int { x ∷ Int, y ∷ Int })
--Spreads explosions
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
--Ticks bombs and explosions (and deletes them if necessary)
bombTick (Bomb { time }) = Bomb { time: time - 1 }

bombTick (Explosion ex@{ existTime }) = if existTime < 0 then Empty else Explosion ex { existTime = existTime - 1 }

bombTick tile = tile

movePlayer :: { xDiff :: Int, yDiff :: Int } -> Reaction World
movePlayer { xDiff, yDiff } = do
  { player: player@{ cords: { x, y } }, board } <- getW
  let
    newPlayerPosition = { x: x + xDiff, y: y + yDiff }
  when (freeTile newPlayerPosition board) $ updateW_ { player: player { cords = newPlayerPosition } }

movePlayer2 ∷ Tuple Enemy Int → Reaction World
movePlayer2 (player2@{ cords: cords@{ x, y }, isOnRun: isOnRun@{ time } } /\ i) = do
  { player2: player2arr } <- getW
  running /\ { xDiff, yDiff } <- enemyDirection player2 -- :: Array (Reaction (Tuple Boolean {xDiff :: Int, yDiff :: Int}))
  let
    newPlayerPosition = { x: x + xDiff, y: y + yDiff }

    newArr = fromMaybe [] $ updateAt i { cords: newPlayerPosition, isOnRun: isOnRun { time = if not running then 0 else time }, lastSeen: cords } player2arr
  updateW_ { player2: newArr }

enemyDirection ∷ ∀ (t337 ∷ Row Type). Enemy → ReactionM { board ∷ Grid Tile | t337 } (Tuple Boolean { xDiff ∷ Int, yDiff ∷ Int })
enemyDirection { cords: cords@{ x: x1, y: y1 }, isOnRun: { running }, lastSeen } = do
  { board } <- getW
  coin <- liftEffect $ randomInt 0 999999
  let
    possibleDirections = Array.filter (wayIsOK cords lastSeen board) directions

    possibleTurns = Array.filter (not isATurn cords lastSeen) possibleDirections
  pure
    $ if isEmpty lastSeen board && (Array.null possibleDirections || (tileIsBomb $ fromMaybe Empty $ Grid.index board cords)) then do
        running /\ { xDiff: lastSeen.x - x1, yDiff: lastSeen.y - y1 }
      else if running && not Array.null possibleTurns then
        false /\ (fromMaybe { xDiff: 0, yDiff: 0 } <<< index possibleTurns) (coin `mod` length possibleTurns)
      else
        running /\ (fromMaybe { xDiff: 0, yDiff: 0 } <<< index possibleDirections) (coin `mod` length possibleDirections)

isATurn ∷ { x ∷ Int, y ∷ Int } → { x ∷ Int, y ∷ Int } → { xDiff ∷ Int, yDiff ∷ Int } → Boolean
--Tells if a way is a turn based on last tile
isATurn { x: currX, y: currY } { x: prevX, y: prevY } { xDiff, yDiff } =
  { x: currX + xDiff, y: currY + yDiff } == { x: prevX, y: prevY }
    || { x: currX + (xDiff * -1), y: currY + (yDiff * -1) }
    == { x: prevX, y: prevY }

isEmpty :: { x :: Int, y :: Int } -> Grid Tile -> Boolean
isEmpty position board = Grid.index board position == Just Empty

tileIsBomb :: Tile -> Boolean
tileIsBomb (Bomb {}) = true

tileIsBomb _ = false

isExplosion :: { x :: Int, y :: Int } -> Grid Tile -> Boolean
isExplosion position board = case Grid.index board position of
  Just (Explosion {}) -> true
  _ -> false

wayIsOK ∷ { x ∷ Int, y ∷ Int } → { x ∷ Int, y ∷ Int } → Grid Tile → { xDiff ∷ Int, yDiff ∷ Int } → Boolean
wayIsOK { x, y } lastSeen board { xDiff, yDiff } =
  if { x: x + xDiff, y: y + yDiff } == lastSeen then
    false
  else
    isEmpty { x: x + xDiff, y: y + yDiff } board

freeTile :: { x :: Int, y :: Int } -> Grid Tile -> Boolean
freeTile position board = isEmpty position board || isExplosion position board

isGonnaExplode :: Tuple { x ∷ Int, y ∷ Int } Tile -> Boolean
isGonnaExplode (_ /\ Bomb { time }) = time == 1

isGonnaExplode _ = false
