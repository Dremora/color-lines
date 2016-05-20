import Effects exposing (Never)
import Signal
import StartApp exposing (start)
import Task
import Time

import ColorLines exposing (init, update, view, input)


app =
  start
    { init = init
    , update = update
    , view = view
    , inputs = [input]
    }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
