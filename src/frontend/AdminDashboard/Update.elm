module AdminDashboard.Update where

import AdminDashboard.Model as AdminDashboard exposing (initialModel, Model)

import Effects exposing (Effects, none)

type Action
  = Activate

update : Action -> AdminDashboard.Model -> (AdminDashboard.Model, Effects Action)
update action model =
  case action of
    Activate ->
      ( model
      , Effects.none
      )
