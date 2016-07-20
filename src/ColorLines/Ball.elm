module ColorLines.Ball exposing (Ball, init, view, startRemoving, update, Msg(..), isRemoved
  , cantRemove)

import Color exposing (Color, red, green, blue, grey)
import Collage exposing (Form, group, circle, filled, outlined, solid, alpha, move)
import Random exposing (Generator)
import Time exposing (Time)
import Platform.Cmd as Cmd exposing (Cmd, none)
import Task exposing (Task)
import Random

import Ease

import ColorLines.Bounce as Bounce


type BallColor = Red | Green | Blue

type Animation =
    Normal
  | Removed
  | Appearing { elapsed: Float }
  | Disappearing { elapsed: Float }
  | CantMove { elapsed: Float }


type alias Ball =
  { color: BallColor
  , selected: Bool
  , animationState: Animation
  }


isRemoved ball =
  ball.animationState == Removed


radius : Float
radius =
  18


ballAppearTime : Time
ballAppearTime =
  Time.second / 3


ballShakeTime : Time
ballShakeTime =
  Time.second


color : Ball -> Color
color ball =
  case ball.color of
    Red -> red
    Green -> green
    Blue -> blue


init : Generator Ball
init =
  let
    intToColor int = case int of
      0 -> Red
      1 -> Green
      2 -> Blue
      _ -> Red
    intToBall int = { color = intToColor int, selected = False, animationState = Appearing { elapsed = 0 } }
  in
    Random.map intToBall (Random.int 0 2)


type Msg = Tick Time


startRemoving : Ball -> Ball
startRemoving ball =
  { ball | animationState = Disappearing { elapsed = 0 } }


cantRemove : Ball -> Ball
cantRemove ball =
  { ball | animationState = CantMove { elapsed = 0 } }


update : Msg -> Ball -> Ball
update msg ball =
  case msg of
    Tick delta ->
      case ball.animationState of
        Appearing { elapsed } ->
          let
            newElapsed = elapsed + delta / ballAppearTime
          in
            if newElapsed < 1 then
              { ball | animationState = Appearing { elapsed = newElapsed } }
            else
              { ball | animationState = Normal }
        Disappearing { elapsed } ->
          let
            newElapsed = elapsed + delta / ballAppearTime
          in
            if newElapsed < 1 then
              { ball | animationState = Disappearing { elapsed = newElapsed } }
            else
              { ball | animationState = Removed }
        CantMove { elapsed } ->
          let
            newElapsed = elapsed + delta / ballShakeTime
          in
            if newElapsed < 1 then
              { ball | animationState = CantMove { elapsed = newElapsed } }
            else
              { ball | animationState = Normal }
        _ ->
          ball


view : Ball -> Form
view ball =
  let
    currentRadius =
      case ball.animationState of
        Normal -> radius
        Removed -> 0
        Appearing { elapsed } -> (Ease.outBack elapsed) * radius
        Disappearing { elapsed } -> (1 + (Ease.inBack elapsed)) * radius
        CantMove _ -> radius
        -- CantMove { elapsed } -> radius + ((Bounce.bounce 300 200) elapsed) * 6

    offsetX =
      case ball.animationState of
        CantMove { elapsed } ->
          (Bounce.bounce 300 200) elapsed * 7
        _ -> 0

    opacity =
      case ball.animationState of
        Disappearing { elapsed } -> 1 - (Ease.inBack elapsed)
        _ -> 1

    contents = filled (color ball) (circle currentRadius)
    border = outlined (solid grey) (circle currentRadius)
    filledBall =
      if ball.selected then
        group [ contents, border ]
      else
        contents
  in
    filledBall |> alpha opacity |> move (offsetX, 0)
