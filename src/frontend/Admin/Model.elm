module Admin.Model where

import TransitRouter exposing (WithRoute)

import Admin.Router exposing (Route)
import AdminDashboard.Model as AdminDashboard exposing (initialModel, Model)
import ArticleList.Model as ArticleList exposing (initialModel, Model)
import ArticleForm.Model as ArticleForm exposing (initialModel, Model)

type alias Model =
  WithRoute Route { dashboard : AdminDashboard.Model
                  , articleList : ArticleList.Model
                  , articleForm : ArticleForm.Model
                  }

initialModel : Route -> Model
initialModel route =
  { transitRouter = TransitRouter.empty route
  , dashboard = AdminDashboard.initialModel
  , articleList = ArticleList.initialModel
  , articleForm = ArticleForm.initialModel
  }
