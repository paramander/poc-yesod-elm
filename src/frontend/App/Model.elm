module App.Model where

import TransitRouter exposing (WithRoute)
import App.Router exposing (Route)
import ArticleList.Model as ArticleList exposing (initialModel, Model)

type alias Model = WithRoute Route { articleList : ArticleList.Model }

initialModel : Route -> Model
initialModel route =
  { transitRouter = TransitRouter.empty route
  , articleList = ArticleList.initialModel
  }
