module AdminDashboard.Model where

type alias Model =
  { username : String
  , userID : Int
  }

initialModel : Model
initialModel =
  { username = "Henk"
  , userID = 1
  }
