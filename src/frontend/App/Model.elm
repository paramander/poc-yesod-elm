module App.Model where

import TransitRouter exposing (WithRoute)
import App.Router exposing (Route)
import ArticleForm.Model as ArticleForm exposing (initialModel, Model)
import ArticleList.Model as ArticleList exposing (initialModel, Model)

type alias Model = WithRoute Route { articleForm : ArticleForm.Model
                                   , articleList : ArticleList.Model
                                   }

initialModel : Route -> Model
initialModel route =
  { transitRouter = TransitRouter.empty route
  , articleForm = ArticleForm.initialModel
  , articleList = ArticleList.initialModel
  }
