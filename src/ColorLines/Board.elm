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
import Matrix exposing (Matrix)

import ColorLines.Ball as BallM exposing (Ball, Notification(..))


type alias Location = (Int, Int)
type alias Board = Matrix (Maybe Ball)


rows = 9
cols = 9


empty = Matrix.repeat rows cols Nothing


positionToLocation : Int -> Location
positionToLocation pos =
  (pos % cols, pos // cols)


updateLocation : Location -> (Maybe Ball -> Maybe Ball) -> Board -> Board
updateLocation (x, y) update board =
  Matrix.update x y update board


getLocation : Location -> Board -> Maybe Ball
getLocation (x, y) board =
  Matrix.get x y board
  |> (flip Maybe.andThen) identity


setLocation : Location -> Maybe Ball -> Board -> Board
setLocation (x, y) ball board =
  Matrix.set x y ball board


clearLocation : Location -> Board -> Board
clearLocation location board =
  setLocation location Nothing board


selectedLocation : Board -> Maybe Location
selectedLocation board =
  board
  |> Matrix.toIndexedArray
  |> Array.filter (\(_, ball) ->
    case ball of
      Just ball -> ball.selected
      Nothing -> False
    )
  |> Array.toList
  |> List.head
  |> Maybe.map fst


clearSelection : Location -> Board -> Board
clearSelection location board =
  updateLocation location (Maybe.map (\ball -> { ball | selected = False })) board

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
    Array.foldl sum 0 (Matrix.filter (\_ -> True) board)


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
    Array.foldl rec (-1, index + 1) (Matrix.filter (\_ -> True) board) |> fst


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
  |> Matrix.toIndexedArray
  |> Array.toList
  |> List.filterMap (\(location, ball) -> case ball of
      Just ball -> Just (location, ball)
      Nothing -> Nothing
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
      case getLocation location board of
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
      getLocation location board |> isNothing

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
      |> Matrix.toIndexedArray
      |> Array.toList
      |> List.filterMap (\(location, ball) -> Maybe.map (\ball -> (location, ball)) ball)

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
    ball = getLocation newLocation board
  in
    case (oldLocation, ball) of
      (_, Just ball) ->
        Maybe.map (\location -> clearSelection location board) oldLocation
        |> Maybe.withDefault board
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
            Nothing ->
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
  case getLocation location board of
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
    case getLocation newLocation board of
      Nothing ->
        []
      Just ball2 ->
        if ball2.color == ball.color then
          newLocation :: findDir ball newLocation board direction
        else
          []
