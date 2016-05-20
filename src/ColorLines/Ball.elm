module ColorLines.Ball (Ball, init, draw, update, Action) where

import Color exposing (Color, red, green, blue, grey)
import Effects
import Graphics.Collage exposing (Form, group, circle, filled, outlined, solid)
import Random exposing (Generator)
import Time exposing (Time)


type BallColor = Red | Green | Blue

type Animation = Static | Hidden | Appearing { elapsedTime: Time, prevClockTime: Time }


type alias Ball =
  { color: BallColor
  , selected: Bool
  , animationState: Animation
  }


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


generator : Generator Ball
generator =
  let
    intToColor int = case int of
      0 -> Red
      1 -> Green
      2 -> Blue
      _ -> Red
    intToBall int = { color = intToColor int, selected = False, animationState = Hidden }
  in
    Random.map intToBall (Random.int 0 2)


type Action = Tick Time


init : Generator (Ball, Effects.Effects Action)
init =
  Random.map (flip (,) (Effects.tick Tick)) generator


update : Action -> Ball -> (Ball, Effects.Effects Action)
update action ball =
  case action of
    Tick clockTime ->
      let
        newElapsedTime =
          case ball.animationState of
            Static -> 0
            Hidden -> 0
            Appearing { elapsedTime, prevClockTime } ->
              elapsedTime + (clockTime - prevClockTime)

        continue = newElapsedTime < ballAppearTime
      in
        if continue then
          ( { ball | animationState = Appearing { elapsedTime = newElapsedTime, prevClockTime = clockTime } }
          , Effects.tick Tick)
        else
          ( { ball | animationState = Static }
          , Effects.none)


draw : Ball -> Form
draw ball =
  let
    currentRadius =
      case ball.animationState of
        Static -> radius
        Hidden -> 0
        Appearing { elapsedTime } -> elapsedTime / ballAppearTime * radius

    contents = filled (color ball) (circle currentRadius)
    border = outlined (solid grey) (circle currentRadius)
  in
    if ball.selected then
      group [ contents, border ]
    else
      contents
