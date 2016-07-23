module ColorLines.Board exposing (
    rows, cols, empty, Location, blockSize, Board, view, animateBoard, select
  , generate3balls
  )

import Array
import Collage exposing (Form, collage, group, rect, filled, circle, move)
import Random exposing (Generator)
import Set exposing (Set)
import Task exposing (Task)
import Time exposing (Time)

import AStar
import Maybe.Extra exposing (isJust, isNothing)

import ColorLines.Ball as BallM exposing (Ball, Notification(..))
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


updateLocation : Location -> (Maybe Ball -> Maybe Ball) -> Board -> Board
updateLocation location update board =
  Array.set (locationToPosition location) (update (getBall location board)) board


setLocation : Location -> Maybe Ball -> Board -> Board
setLocation location ball board =
  Array.set (locationToPosition location) ball board


clearLocation : Location -> Board -> Board
clearLocation location board =
  setLocation location Nothing board


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


startMovingBall : List Location -> Location -> Location -> Board -> Board
startMovingBall path old new board =
  let
    (x, y) = old
    -- calcDiff (newX, newY) ((oldX, oldY), diffs) =
    --   ((newX, newY), (newX - oldX, newY - oldY) :: diffs)
    -- diffPath = List.foldr calcDiff (old, []) path |> snd |> Array.fromList
    calcDiff (newX, newY) ((oldX, oldY), diffs) =
      ((newX, newY), ((oldX - x, oldY - y), (newX - x, newY - y)) :: diffs)
    diffPath = List.foldl calcDiff (old, []) path |> snd |> List.reverse
    -- calcDiff (newX, newY) = (newX - x, newY - y)
    -- diffPath = List.map calcDiff (old :: path) |> Array.fromList
    updateBall ball =
      ball
      |> Maybe.map (\ball -> { ball | selected = False })
      |> Maybe.map (\ball -> BallM.moveViaPath ball diffPath new)
  in
    updateLocation old updateBall board


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


generate3balls board seed =
  (board, Random.initialSeed seed)
  |> addRandomBall |> addRandomBall |> addRandomBall |> fst


shakeBall : Location -> Board -> Board
shakeBall location board =
  updateLocation location (Maybe.map BallM.cantRemove) board


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
            setLocation location (Just ball') board
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


findPath : Board -> Location -> Location -> Maybe (List Location)
findPath board from to =
  AStar.findPath AStar.straightLineCost (moves board) from to


animateBoard : Time -> Int -> Board -> Board
animateBoard delta seed board =
  let
    actionOn (location, ball) =
      case BallM.animate delta ball of
        Nop -> identity
        JustAnimate ball -> setLocation location (Just ball)
        Remove -> clearLocation location
        FinalizeMove ball newLocation ->
          \board ->
            let
              board' =
                board
                |> clearLocation location
                |> setLocation newLocation (Just ball)
              toRemove = findMatching newLocation board'
            in
              if List.isEmpty toRemove then
                generate3balls board' seed
              else
                removeBalls toRemove board'
    ballsWithLocations =
      board
      |> Array.toIndexedList
      |> List.filterMap (\(index, ball) -> Maybe.map (\ball -> (positionToLocation index, ball)) ball)

    actions =
      List.map actionOn ballsWithLocations

    foldFun acc action = action << acc
  in
    (List.foldl foldFun identity actions) board


isAnimating board =
  board
  |> Array.toList
  |> List.filterMap identity
  |> List.any BallM.isAnimating


select : Board -> Location -> Board
select board newLocation =
  let
    oldLocation = selectedLocation board
    ball = getBall newLocation board
  in
    case (oldLocation, ball) of
      (_, Just ball) ->
        board
        |> clearSelection
        |> setLocation newLocation (Just { ball | selected = True })

      (Just oldLocation, Nothing) ->
        let
          path = findPath board oldLocation newLocation
        in
          case path of
            Just path ->
              let
                board' = startMovingBall path oldLocation newLocation board
                toRemove = findMatching newLocation board'
              in
                board'
            _ ->
              shakeBall oldLocation board

      _ ->
        board


addRandomBall : (Board, Random.Seed) -> (Board, Random.Seed)
addRandomBall (board, seed) =
  let
    (ball, location, seed') = randomFreeLocation board seed
  in
    ((setLocation location (Just ball) board, seed'))


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
