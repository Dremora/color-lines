module ColorLines.Board exposing (
    rows, cols, empty, Location, blockSize, Msg(..), Board, getBall, view, update
  )

import Array
import Debug
import Collage exposing (Form, collage, group, rect, filled, circle, move)
import Random exposing (Generator)
import Set exposing (Set)
import Task exposing (Task)
import Time

import AStar
import Maybe.Extra exposing (isJust, isNothing)

import ColorLines.Ball as BallM exposing (Ball)
import ColorLines.Matrix as Matrix


type alias Location = (Int, Int)
type alias Board = Array.Array (Maybe Ball)


rows = 9
cols = 9


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


randomFreeLocation : Board -> Random.Seed -> (Ball, Location, Random.Seed)
randomFreeLocation board seed =
  let
    dimGenerator = Random.int 0 <| (emptyCellCount board) - 1
    composed = Random.map2 (,) dimGenerator BallM.init
    ((index, ball), seed') = Random.step composed seed
  in
    (ball, positionToLocation <| freeIndexToPosition board <| index, seed')


blockSize = 40


view : Board -> List Form
view board =
  board
  |> Array.toIndexedList
  |> List.filterMap (\(position, ball) -> case ball of
      Nothing -> Nothing
      Just ball -> Just ((positionToLocation position), ball)
    )
  |> List.map drawBallAtLocation


drawBallAtLocation : (Location, Ball) -> Form
drawBallAtLocation ((x, y), ball) =
  let
    tX = (toFloat x + 0.5) * blockSize
    tY = -(toFloat y + 0.5) * blockSize
  in
    ball
    |> BallM.view
    |> move (tX, tY)


type Msg =
  BallAction Location BallM.Msg
  | Generate Int
  | Select Int Location
  | Tick Time.Time


generate3balls board seed =
  let
    (board', _) =
      addRandomBall (board, Random.initialSeed seed)
      |> addRandomBall |> addRandomBall
  in
    board'


remove : Maybe Ball -> Maybe Ball
remove ballM =
  Maybe.andThen ballM (\ball -> if BallM.isRemoved ball then Nothing else Just ball)


shakeBall : Location -> Board -> Board
shakeBall location board =
  getBall location board
  |> Maybe.map BallM.cantRemove
  |> Maybe.map (\ball -> updateLocation location (Just ball) board)
  |> Maybe.withDefault board


removeBalls : List Location -> Board -> Board
removeBalls toRemove board =
  let
    rec location board =
      case getBall location board of
        Nothing -> board
        Just ball ->
          let
            ball' = BallM.startRemoving ball
          in
            updateLocation location (Just ball') board
  in
    List.foldl rec (board) toRemove


moves : Board -> Location -> Set Location
moves board location =
  let
    allNeighbours : Location -> List Location
    allNeighbours (x, y) =
      [ (x - 1, y)
      , (x + 1, y)
      , (x, y - 1)
      , (x, y + 1)
      ]

    isLocationOnBoard : Location -> Bool
    isLocationOnBoard (x, y) =
      x >= 0 && y >= 0 && x < cols && y < rows

    isLocationEmpty : Location -> Bool
    isLocationEmpty location =
      getBall location board |> isNothing

  in
    allNeighbours location
    |> List.filter isLocationOnBoard
    |> List.filter isLocationEmpty
    |> Set.fromList


canMove : Board -> Location -> Location -> Bool
canMove board from to =
  isJust (AStar.findPath AStar.straightLineCost (moves board) from to)


update : Msg -> Board -> (Board, Cmd Msg)
update action board =
  case action of
    Generate seed ->
      (generate3balls board seed, Cmd.none)

    Tick delta ->
      let
        updateBall = BallM.update (BallM.Tick delta)
        balls = board
          |> Array.map (Maybe.map updateBall)
          |> Array.map remove
      in
        (balls, Cmd.none)

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
            , Cmd.none)

          (Just oldLocation, Nothing) ->
            let
              board' = moveBall oldLocation newLocation board
              toRemove = findMatching newLocation board'
            in
              if not (canMove board oldLocation newLocation) then
                (shakeBall oldLocation board, Cmd.none)
              else if List.isEmpty toRemove then
                (generate3balls board' seed, Cmd.none)
              else
                (removeBalls toRemove board', Cmd.none)

          _ ->
            (board, Cmd.none)

    BallAction location ballAction ->
      case getBall location board of
        Just ball ->
          let
            ball' = BallM.update ballAction ball
          in
            ( updateLocation location (Just ball') board
            , Cmd.none)
        Nothing  ->
          (board, Cmd.none)


addRandomBall : (Board, Random.Seed) -> (Board, Random.Seed)
addRandomBall (board, seed) =
  let
    (ball, location, seed') = randomFreeLocation board seed
  in
    ((updateLocation location (Just ball) board, seed'))


type alias Direction = (Int, Int)


findMatching : Location -> Board -> List Location
findMatching location board =
  case getBall location board of
    Nothing ->
      []
    Just ball ->
      let
        find = findDir ball location board
        horizontal = find (0, -1) `List.append` find (0, 1)
        vertical = find (-1, 0) `List.append` find (1, 0)
        topleft = find (-1, -1) `List.append` find (1, 1)
        topright = find (1, -1) `List.append` find (-1, 1)
        required = \list -> if List.length list >= 4 then list else []
        all =
          required horizontal `List.append`
          required vertical `List.append`
          required topleft `List.append`
          required topright
      in
        if List.isEmpty all then [] else location :: all


findDir : Ball -> Location -> Board -> Direction -> List Location
findDir ball ((x, y) as location) board ((dx, dy) as direction) =
  let
    newLocation = (x + dx, y + dy)
  in
    case getBall newLocation board of
      Nothing ->
        []
      Just ball2 ->
        if ball2.color == ball.color then
          newLocation :: findDir ball newLocation board direction
        else
          []
