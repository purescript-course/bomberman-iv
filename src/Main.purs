module Main
  ( Enemy(..)
  , Tile(..)
  , Timer(..)
  , World
  , bombBoom
  , bombTick
  , directions
  , draw
  , enemyDirection
  , evenWallPlacement
  , explode
  , handleEvent
  , height
  , isBomb
  , isBox
  , isGonnaExplode
  , isWall
  , main
  , movePlayer
  , movePlayer2
  , radiusConst
  , runningSpeed
  , setTile
  , timer
  , walkingSpeed
  , wayIsOK
  , width
  ) where

import Prelude
import Data.Array ((!!))
import Data.Array as Array
import Data.Grid (Grid, Coordinates, enumerate)
import Data.Grid as Grid
import Data.Int (toNumber)
import Data.List (List(..), (:), null, filter, concat, fromFoldable, find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Math (sqrt, pow)
import Reactor (executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors (blue400, gray300, gray600, gray500, hsl, red600, green700)
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Internal.Widget (Widget(..))
import Reactor.Reaction (Reaction, ReactionM)

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
    initial = { player: { x: 1, y: 1 }, player2: Enemy { cords: { x: width - 2, y: height - 2 }, isOnRun: Timer { running: false, time: 0 }, lastSeen: { x: width - 2, y: height - 2 } }, board, tickCounter: 0 }
  let
    reactor = { initial, draw, handleEvent, isPaused: const false }
  runReactor reactor { title: "Bomberman", width, height, widgets: [ Tuple "Sexy widget" (Section { title: "Sexy" }) ] }

data Tile
  = Wall
  | Box
  | Bomb { time :: Int }
  | Explosion { existTime :: Int, distance :: Int }
  | Empty

data Enemy
  = Enemy { cords :: Coordinates, isOnRun :: Timer, lastSeen :: Coordinates }

data Timer
  = Timer { running :: Boolean, time :: Int }

derive instance tileEq :: Eq Tile

type World
  = { player :: Coordinates, player2 :: Enemy, board :: Grid Tile, tickCounter :: Int }

directions ∷ Array { xDiff ∷ Int, yDiff ∷ Int }
directions = [ { xDiff: -1, yDiff: 0 }, { xDiff: 1, yDiff: 0 }, { xDiff: 0, yDiff: 1 }, { xDiff: 0, yDiff: -1 } ]

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
draw { player, player2: Enemy { cords }, board } = do
  drawGrid board drawTile
  fill blue400 $ tile player
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

      hue = (60.0 - sqrt (60.0 * 60.0 / exRadiusNum * b))

      saturation = (100.0 - ((division * b) * pow (division * b) 2.0)) / 100.0

      light = (50.0 - sqrt (18.0 * 18.0 / exRadiusNum * b)) / 100.0
    in
      hsl hue saturation light

handleEvent :: Event -> Reaction World
handleEvent event = do
  { player: { x, y }, board, player2: Enemy { cords, isOnRun: Timer { running, time }, lastSeen }, tickCounter } <- getW
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
    Tick {} ->
      let
        bombsTicked = bombTick <$> board
      in
        do
          coin <- liftEffect $ randomInt 0 999999 --hehe; abychom mohli použít jeden coin na více věcí, dělám to takhle.
          let
            tickedPlayer2 = Enemy { cords, isOnRun: Timer { running: time /= 0, time: if time /= 0 then time - 1 else time }, lastSeen }
          updateW_ { board: bombsTicked, tickCounter: tickCounter + 1, player2: tickedPlayer2 } --just ticking all the clocks
          when (coin `mod` 5 < 4 && tickCounter `mod` (if running then runningSpeed else walkingSpeed) == 0) movePlayer2 --p = 0,8
          if coin `mod` 100 < 69 && tickCounter `mod` 300 == 0 then --p = 0,7
            let
              bombPlanted = Grid.updateAt' cords (Bomb { time: timer }) board

              newPlayer2 = Enemy { cords, isOnRun: Timer { running: true, time: radiusConst * runningSpeed }, lastSeen }
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
          updateW_ { board: Grid.construct width height customSetTile }
    _ -> executeDefaultBehavior

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
      Bomb {} -> if null $ filter (_ == tile) bombsDonee then Tuple 0 tile : bombBoom board_ tile radiusConst (tile : bombsDonee) else Nil
      Box -> (Tuple (distanceFromCords tile) tile) : Nil
      _ ->
        if not (radius_ == 0) then
          Tuple (distanceFromCords tile) tile : go board_ { x: expX + xChange, y: expY + yChange } (radius_ - 1) enum bombsDonee
        else
          Nil

    distanceFromCords a = absoluteValue (a.x - x) + absoluteValue (a.y - y)

    absoluteValue i = if i < 0 then i * -1 else i
  in
    (Tuple 0 { x, y }) {- bomb cords -} : right <> left <> up <> down

bombTick :: Tile -> Tile
bombTick (Bomb { time }) = Bomb { time: time - 1 }

bombTick (Explosion { existTime, distance }) = if existTime < 0 then Empty else Explosion { existTime: existTime - 1, distance }

bombTick tile = tile

movePlayer :: { xDiff :: Int, yDiff :: Int } -> Reaction World
movePlayer { xDiff, yDiff } = do
  { player: { x, y }, board } <- getW
  let
    newPlayerPosition = { x: x + xDiff, y: y + yDiff }
  when (isEmpty newPlayerPosition board)
    $ updateW_ { player: newPlayerPosition }

movePlayer2 :: Reaction World
movePlayer2 = do
  Tuple running { xDiff, yDiff } <- enemyDirection
  { player2: Enemy { cords: cords@{ x, y }, isOnRun: Timer { time } } } <- getW
  let
    newPlayerPosition = { x: x + xDiff, y: y + yDiff }
  updateW_ { player2: Enemy { cords: newPlayerPosition, isOnRun: Timer { running, time: if not running then 0 else time }, lastSeen: cords } }

enemyDirection ∷ ReactionM World (Tuple Boolean { xDiff ∷ Int, yDiff ∷ Int })
enemyDirection = do
  { board, player2: Enemy { cords: cords@{ x: x1, y: y1 }, isOnRun: Timer { running }, lastSeen } } <- getW
  coin <- liftEffect $ randomInt 0 999999
  let
    possibleDirections = Array.filter (wayIsOK cords lastSeen board) directions
  let
    possibleTurns = Array.filter (not isATurn cords lastSeen) possibleDirections
  pure
    $ if isEmpty lastSeen board && (Array.null possibleDirections || (isBomb <<< fromMaybe Empty $ Grid.index board cords)) then do
        Tuple running $ { xDiff: lastSeen.x - x1, yDiff: lastSeen.y - y1 }
      else if running && not Array.null possibleTurns then
        Tuple false $ fromMaybe { xDiff: 0, yDiff: 0 } $ possibleTurns !! (coin `mod` Array.length possibleTurns)
      else
        Tuple running $ fromMaybe { xDiff: 0, yDiff: 0 } $ possibleDirections !! (coin `mod` Array.length possibleDirections)

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
