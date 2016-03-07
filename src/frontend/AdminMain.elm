import StartApp

import Effects exposing (Never)
import Task
import Signal

import Admin.Router exposing (decode, Route)
import Admin.Update exposing (init, update, actions)
import Admin.View exposing (view)

app =
  StartApp.start { init = Admin.Update.init initialPath
                 , update = Admin.Update.update
                 , view = Admin.View.view
                 , inputs = [ actions ]
                 }

main =
  app.html

port initialPath : String

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
