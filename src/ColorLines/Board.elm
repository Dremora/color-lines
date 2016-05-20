module ColorLines.Board (
  Location, Board, rows, cols, empty, getBall,
  draw, blockSize, Action(Generate, Select), update
  ) where

import Array
import Debug
import Graphics.Collage exposing (Form, collage, group, rect, filled, circle, move)
import Effects
import Random exposing (Generator)

import ColorLines.Ball as BallM exposing (Ball)


type alias Location = (Int, Int)
type alias Board = Array.Array (Maybe Ball)


rows = 5
cols = 13


empty = Array.repeat (rows * cols) Nothing


locationToPosition : Location -> Int
locationToPosition (x, y) =
  y * cols + x


positionToLocation : Int -> Location
positionToLocation pos =
  (pos % cols, pos // cols)


updateLocation : Location -> Maybe Ball -> Board -> Board
updateLocation location ball board =
  Array.set (locationToPosition location) ball board


clearBall : Location -> Board -> Board
clearBall location board =
  updateLocation location Nothing board


getBall : Location -> Board -> Maybe Ball
getBall location board =
  Array.get (locationToPosition location) board
  |> (flip Maybe.andThen) identity


selectedLocation : Board -> Maybe Location
selectedLocation board =
  Array.toIndexedList board
  |> List.filter (\(_, ball) ->
    case ball of
      Just ball -> ball.selected
      Nothing -> False
    )
  |> List.head
  |> Maybe.map (positionToLocation << fst)


clearSelection : Board -> Board
clearSelection =
  Array.map <| Maybe.map (\ball -> { ball | selected = False })


moveBall : Location -> Location -> Board -> Board
moveBall old new board =
  let
    ball = Maybe.map (\ball -> { ball | selected = False }) (getBall old board)
  in
    board
    |> clearBall old
    |> updateLocation new ball


hasBallAt : Location -> Board -> Bool
hasBallAt location board =
  case Array.get (locationToPosition location) board of
    Just _ -> True
    Nothing -> False


emptyCellCount : Board -> Int
emptyCellCount board =
  let
    sum x acc =
      case x of
        Just _ -> acc
        Nothing -> acc + 1
  in
    Array.foldl sum 0 board


freeIndexToPosition : Board -> Int -> Int
freeIndexToPosition board index =
  let
    rec val (pos, left) =
      if left == 0 then
        (pos, left)
      else case val of
        Just _ -> (pos + 1, left)
        Nothing -> (pos + 1, left - 1)
  in
    Array.foldl rec (-1, index + 1) board |> fst


randomFreeLocation : Board -> Random.Seed -> (Ball, Location, Random.Seed, Effects.Effects BallM.Action)
randomFreeLocation board seed =
  let
    dimGenerator = Random.int 0 <| (emptyCellCount board) - 1
    composed = Random.map2 (,) dimGenerator BallM.init
    ((index, (ball, effects)), seed') = Random.generate composed seed
  in
    (ball, positionToLocation <| freeIndexToPosition board <| index, seed', effects)


blockSize = 40


draw : Board -> List Form
draw board =
  board
  |> Array.toIndexedList
  |> List.filterMap (\(position, ball) -> Maybe.map (\ball -> (positionToLocation position, ball)) ball)
  |> List.map drawBallAtLocation


drawBallAtLocation : (Location, Ball) -> Form
drawBallAtLocation ((x, y), ball) =
  let
    tX = (toFloat x + 0.5) * blockSize
    tY = -(toFloat y + 0.5) * blockSize
  in
    ball
    |> BallM.draw
    |> move (tX, tY)


type Action =
  BallAction Location BallM.Action
  | Generate Int
  | Select Int Location


generate3balls board seed =
  let
    ((board', seed'), location, action') = addRandomBall (board, Random.initialSeed seed)
    ((board'', seed''), location', action'') = addRandomBall (board', seed')
    ((board''', seed'''), location'', action''') = addRandomBall (board'', seed'')
    actions = Effects.batch [
      Effects.map (BallAction location) action'
    , Effects.map (BallAction location') action''
    , Effects.map (BallAction location'') action'''
    ]
  in
    (board''', actions)

update : Action -> Board -> (Board, Effects.Effects Action)
update action board =
  case action of
    Generate seed ->
      generate3balls board seed

    Select seed newLocation ->
      let
        oldLocation = selectedLocation board
        ball = getBall newLocation board
      in
        case (oldLocation, ball) of
          (_, Just ball) ->
            (board
            |> clearSelection
            |> updateLocation newLocation (Just { ball | selected = True })
            , Effects.none)

          (Just oldLocation, Nothing) ->
            let
              board' = moveBall oldLocation newLocation board
            in
              generate3balls board' seed

          _ ->
            (board, Effects.none)

    BallAction location ballAction ->
      case getBall location board of
        Just ball ->
          let
            (ball', effects) = BallM.update ballAction ball
          in
            ( updateLocation location (Just ball') board
            , Effects.map (BallAction location) effects)
        Nothing  ->
          (board, Effects.none)


addRandomBall : (Board, Random.Seed) -> ((Board, Random.Seed), Location, Effects.Effects BallM.Action)
addRandomBall (board, seed) =
  let
    (ball, location, seed', action) = randomFreeLocation board seed
  in
    ((updateLocation location (Just ball) board, seed'), location, action)


-- findMatchingLocationsInRow : Board -> Int -> List Location
-- findMatchingLocationsInRow board row =



-- findHorizontalMatches : Board -> List Location
-- findMatches board =
--   Debug.crash "not implemented"
--
--
-- findMatches : Board -> List Location
-- findMatches board =
--   Debug.crash "not implemented"