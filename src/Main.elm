import Html.App as Html
import ColorLines exposing (init, update, view, subscriptions)


main =
  Html.program
    { init = init, update = update, view = view, subscriptions = subscriptions }
