module ColorLines.Ball exposing (Ball, init, view, startRemoving, animate
  , cantRemove, moveViaPath, isAnimating, Notification(..))

import Array exposing (Array)
import Color exposing (Color, red, green, blue, grey)
import Collage exposing (Form, group, circle, filled, outlined, solid, alpha, move)
import Random exposing (Generator)
import Time exposing (Time)
import Platform.Cmd as Cmd exposing (Cmd, none)
import Task exposing (Task)
import Random

import Animation exposing (Animation)
import Ease

import ColorLines.Bounce as Bounce


type BallColor = Red | Green | Blue
type alias Location = (Int, Int)

type AnimationState =
    Normal
  | Appearing { elapsed: Float }
  | Disappearing { elapsed: Float }
  | Moving { animation: Animation (Float, Float), finalLocation: Location }
  | CantMove { elapsed: Float }


type alias Ball =
  { color: BallColor
  , selected: Bool
  , animationState: AnimationState
  }


isAnimating ball =
  ball.animationState /= Normal


radius : Float
radius =
  18


ballAppearTime : Time
ballAppearTime =
  Time.second / 3


ballShakeTime : Time
ballShakeTime =
  Time.second


ballMoveTime : Time
ballMoveTime =
  Time.second * 2


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


startRemoving : Ball -> Ball
startRemoving ball =
  { ball | animationState = Disappearing { elapsed = 0 } }


cantRemove : Ball -> Ball
cantRemove ball =
  { ball | animationState = CantMove { elapsed = 0 } }


moveViaPath : Ball -> List ((Int, Int), (Int, Int)) -> Location -> Ball
moveViaPath ball path location =
  let
    segmentToAnimation ((fromX, fromY), (toX, toY)) =
      let
        animateFrom i =
          (toFloat fromX + toFloat (toX - fromX) * i)
        animateTo i =
          (toFloat fromY + toFloat (toY - fromY) * i)
      in
        Animation.interval (Time.second / 15)
        |> Animation.map (\i -> (animateFrom i, animateTo i))
    animations = List.map segmentToAnimation path
    animation = List.foldr Animation.append (Animation.immediately (0, 0)) animations
  in
    { ball | animationState = Moving { animation = animation, finalLocation = location } }



type Notification =
  Remove | JustAnimate Ball | FinalizeMove Ball Location | Nop


animate : Time -> Ball -> Notification
animate delta ball =
  case ball.animationState of
    Appearing { elapsed } ->
      let
        newElapsed = elapsed + delta / ballAppearTime
      in
        if newElapsed < 1 then
          JustAnimate { ball | animationState = Appearing { elapsed = newElapsed } }
        else
          JustAnimate { ball | animationState = Normal }
    Disappearing { elapsed } ->
      let
        newElapsed = elapsed + delta / ballAppearTime
      in
        if newElapsed < 1 then
          JustAnimate { ball | animationState = Disappearing { elapsed = newElapsed } }
        else
          Remove
    CantMove { elapsed } ->
      let
        newElapsed = elapsed + delta / ballShakeTime
      in
        if newElapsed < 1 then
          JustAnimate { ball | animationState = CantMove { elapsed = newElapsed } }
        else
          JustAnimate { ball | animationState = Normal }
    Moving { animation, finalLocation } ->
      let
        newAnimation = Animation.run delta animation
      in
        if Animation.isDone newAnimation then
          FinalizeMove { ball | animationState = Normal } finalLocation
        else
          JustAnimate { ball | animationState = Moving { animation = newAnimation, finalLocation = finalLocation } }
    _ ->
      Nop


view : Ball -> Form
view ball =
  let
    currentRadius =
      case ball.animationState of
        Normal -> radius
        Appearing { elapsed } -> (Ease.outBack elapsed) * radius
        Disappearing { elapsed } -> (1 + (Ease.inBack elapsed)) * radius
        CantMove _ -> radius
        _ -> radius
        -- CantMove { elapsed } -> radius + ((Bounce.bounce 300 200) elapsed) * 6

    offsetX =
      case ball.animationState of
        Moving { animation } ->
          (Animation.sample animation |> fst) * 40 -- TODO dehardcode
        CantMove { elapsed } ->
          (Bounce.bounce 300 200) elapsed * 7
        _ -> 0

    offsetY =
      case ball.animationState of
        Moving { animation } ->
          (Animation.sample animation |> snd) * 40 -- TODO dehardcode
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
    filledBall |> alpha opacity |> move (offsetX, -offsetY)
