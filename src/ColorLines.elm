module ColorLines exposing (init, update, view, subscriptions)

import Color exposing (lightRed)
import Collage exposing (Form, collage, group, rect, filled, circle, move)
import Html
import Element
import Random
import Time
import Html.App
import Platform.Cmd as Cmd exposing (Cmd)
import AnimationFrame
import Mouse
import Time exposing (Time)

import ColorLines.Board as Board exposing (Board, Location)


type Msg =
    Generate Int
  | Click Location
  | Tick Time
  | Animate Time Int


init : (Board, Cmd Msg)
init =
  (
    Board.empty
  , Random.generate Generate (Random.int Random.minInt Random.maxInt)
  )


update : Msg -> Board -> (Board, Cmd Msg)
update msg board =
  case msg of
    Generate seed ->
      ( Board.generate3balls board seed, Cmd.none)

    Click location ->
      ( Board.select board location, Cmd.none)

    Tick delta ->
      ( board, (Random.generate (Animate delta) (Random.int Random.minInt Random.maxInt)))

    Animate delta seed ->
      ( Board.animateBoard delta seed board, Cmd.none)


fieldWidth = Board.cols * Board.blockSize
fieldHeight = Board.rows * Board.blockSize

view : Board -> Html.Html Msg
view board =
  collage fieldWidth fieldHeight [
    rect fieldWidth fieldHeight
    |> filled lightRed
    , Board.view board
    |> group
    |> move (-fieldWidth / 2, fieldHeight / 2)
  ]
  |> Element.toHtml


clicks : Sub (Int, Int)
clicks =
  Mouse.clicks (\{ x, y } -> (x, y))


cellClicks : Sub Location
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
