module ColorLines.Ball exposing (Ball, init, view, startRemoving, update, Msg(..), isRemoved
  , cantRemove)

import Color exposing (Color, red, green, blue, grey)
import Collage exposing (Form, group, circle, filled, outlined, solid, alpha)
import Random exposing (Generator)
import Time exposing (Time)
import Platform.Cmd as Cmd exposing (Cmd, none)
import Task exposing (Task)
import Random


type BallColor = Red | Green | Blue

type Animation =
    Normal
  | Removed
  | Appearing { elapsedTime: Time }
  | Disappearing { elapsedTime: Time }
  | CantMove { elapsedTime: Time }


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
  Time.second / 4


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
    intToBall int = { color = intToColor int, selected = False, animationState = Appearing { elapsedTime = 0 } }
  in
    Random.map intToBall (Random.int 0 2)


type Msg = Tick Time


startRemoving : Ball -> Ball
startRemoving ball =
  { ball | animationState = Disappearing { elapsedTime = 0 } }


cantRemove : Ball -> Ball
cantRemove ball =
  { ball | animationState = CantMove { elapsedTime = 0 } }


update : Msg -> Ball -> Ball
update msg ball =
  case msg of
    Tick delta ->
      let
        newElapsedTime =
          case ball.animationState of
            Appearing { elapsedTime } ->
              elapsedTime + delta
            Disappearing { elapsedTime } ->
              elapsedTime + delta
            CantMove { elapsedTime } ->
              elapsedTime + delta
            _ -> 0

        continue = newElapsedTime < ballAppearTime
      in
        case ball.animationState of
          Appearing _ ->
            if continue then
              { ball | animationState = Appearing { elapsedTime = newElapsedTime } }
            else
              { ball | animationState = Normal }
          Disappearing _ ->
            if continue then
              { ball | animationState = Disappearing { elapsedTime = newElapsedTime } }
            else
              { ball | animationState = Removed }
          CantMove _ ->
            if continue then
              { ball | animationState = CantMove { elapsedTime = newElapsedTime } }
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
        Appearing { elapsedTime } -> elapsedTime / ballAppearTime * radius
        Disappearing { elapsedTime } -> (ballAppearTime + elapsedTime) / ballAppearTime * radius
        CantMove { elapsedTime } -> radius + elapsedTime / ballAppearTime * 6

    opacity =
      case ball.animationState of
        Disappearing { elapsedTime } -> (ballAppearTime - elapsedTime) / ballAppearTime
        _ -> 1

    contents = filled (color ball) (circle currentRadius)
    border = outlined (solid grey) (circle currentRadius)
    filledBall =
      if ball.selected then
        group [ contents, border ]
      else
        contents
  in
    filledBall |> alpha opacity
