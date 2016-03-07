module AdminDashboard.View where

import AdminDashboard.Model as AdminDashboard exposing (Model)
import AdminDashboard.Update exposing (Action)

import Html exposing (..)
import Html.Attributes exposing (..)

type alias Model = AdminDashboard.Model

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "wrapper" ]
    [ h2
      [ class "welcome-text" ]
      [ text ("Welcome " ++ model.username) ]
    ]
