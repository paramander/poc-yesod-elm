module App.Model where

import TransitRouter exposing (WithRoute)
import App.Router exposing (Route)
import ArticleForm.Model as ArticleForm exposing (initialModel, Model)
import ArticleList.Model as ArticleList exposing (initialModel, Model)

type alias Model = WithRoute Route { articleForm : ArticleForm.Model
                                   , articleList : ArticleList.Model
                                   }

initialModel : Model
initialModel = { transitRouter = TransitRouter.empty App.Router.EmptyRoute
               , articleForm = ArticleForm.initialModel
               , articleList = ArticleList.initialModel
               }
