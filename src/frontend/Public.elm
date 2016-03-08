import StartApp
import Effects exposing (Never)
import Task
import Signal
import App.Update exposing (init, update, actions)
import App.View exposing (view)

app =
  StartApp.start
            { init = init initialPath
            , update = update
            , view = view
            , inputs = [ actions ]
            }

main =
  app.html

port initialPath : String

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
