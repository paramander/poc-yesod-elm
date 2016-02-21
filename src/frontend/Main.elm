import StartApp

import Effects exposing (Never)
import Task
import Signal

import App.Update exposing (init, update, actions)
import App.View exposing (view)

app =
  StartApp.start { init = App.Update.init initialPath
                 , update = App.Update.update
                 , view = App.View.view
                 , inputs = [ actions ]
                 }

main =
  app.html

port initialPath : String

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
