module ColorLines (init, update, view, input) where

import Color exposing (lightRed)
import Effects
import Graphics.Collage exposing (Form, collage, group, rect, filled, circle, move)
import Html
import Mouse
import Random
import Signal exposing (Address)
import Time
import Debug

import Native.ColorLines
import ColorLines.Board as Board


type alias Game =
  { board: Board.Board
  , seed: Random.Seed
  }


type Action = Generate Float | Click (Float, Board.Location) | SubMsg Board.Action


initialSeed : Int
initialSeed =
  Native.ColorLines.initialSeed


init : (Game, Effects.Effects Action)
init =
  (
    { board = Board.empty
    , seed = Random.initialSeed initialSeed
    }
  , Effects.tick Generate
  )


update : Action -> Game -> (Game, Effects.Effects Action)
update action game =
  case action of
    Generate time ->
      let
        (board, effects) = Board.update (Board.Generate (round time)) game.board
      in
        ( { game | board = board }
        , Effects.map SubMsg effects)

    Click (time, location) ->
      let
        (board, effects) = Board.update (Board.Select (round time) location) game.board
      in
        ( { game | board = board }
        , Effects.map SubMsg effects)

    SubMsg boardAction ->
      let
        (board, effects) = Board.update boardAction game.board
      in
        ( { game | board = board }
        , Effects.map SubMsg effects)


fieldWidth = Board.cols * Board.blockSize
fieldHeight = Board.rows * Board.blockSize


view : Address action -> Game -> Html.Html
view _ game =
  collage fieldWidth fieldHeight [
    rect fieldWidth fieldHeight
    |> filled lightRed,
    Board.draw game.board
    |> group
    |> move (-fieldWidth / 2, fieldHeight / 2)
  ]
  |> Html.fromElement


clicks : Signal (Int, Int)
clicks =
  Signal.sampleOn Mouse.clicks Mouse.position


cellClicks : Signal Board.Location
cellClicks =
  let
    coorsToLocation (x, y) =
      (x // Board.blockSize, y // Board.blockSize)
  in
    Signal.map coorsToLocation clicks


input =
  Signal.map Click <| Time.timestamp cellClicks
