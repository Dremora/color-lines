module ColorLines exposing (init, update, view, subscriptions)

import Color exposing (lightRed)
import Collage exposing (Form, collage, group, rect, filled, circle, move)
import Html
import Element
import Random
import Time
import Debug
import Html.App
import Platform.Cmd as Cmd exposing (Cmd)
import AnimationFrame
import Mouse
import Time

import ColorLines.Board as Board


type alias Game =
  { board: Board.Board
  }


type Msg = BoardMsg Board.Msg
  | Generate Int
  | Click (Board.Location)
  | SelectOrMove (Int, Board.Location)
  | Tick Time.Time


init : (Game, Cmd Msg)
init =
  (
    { board = Board.empty
    }
    , Random.generate Generate (Random.int Random.minInt Random.maxInt)
  )
--
--
update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    Generate seed ->
      let
        (board, effects) = Board.update (Board.Generate seed) game.board
      in
        ( { game | board = board }
        , Cmd.map BoardMsg effects)

    Click location ->
      (game, (Random.generate (\t -> SelectOrMove (t, location)) (Random.int Random.minInt Random.maxInt)))

    SelectOrMove (seed, location) ->
      let
        (board, effects) = Board.update (Board.Select seed location) game.board
      in
        ( { game | board = board }
        , Cmd.map BoardMsg effects)

    BoardMsg boardMsg ->
      let
        (board, effects) = Board.update boardMsg game.board
      in
        ( { game | board = board }
        , Cmd.map BoardMsg effects)

    Tick delta ->
      let
        (board, effects) = Board.update (Board.Tick delta) game.board
      in
        ( { game | board = board }
        , Cmd.map BoardMsg effects)


fieldWidth = Board.cols * Board.blockSize
fieldHeight = Board.rows * Board.blockSize

view : Game -> Html.Html Msg
view game =
  collage fieldWidth fieldHeight [
    rect fieldWidth fieldHeight
    |> filled lightRed
    , Board.view game.board
    |> group
    |> move (-fieldWidth / 2, fieldHeight / 2)
  ]
  |> Element.toHtml


clicks : Sub (Int, Int)
clicks =
  Mouse.clicks (\{ x, y } -> (x, y))


cellClicks : Sub Board.Location
cellClicks =
  let
    coorsToLocation (x, y) =
      (x // Board.blockSize, y // Board.blockSize)
  in
    Sub.map coorsToLocation clicks


subscriptions _ =
  Sub.batch
    [ Sub.map Click cellClicks
    , AnimationFrame.diffs Tick
    ]
