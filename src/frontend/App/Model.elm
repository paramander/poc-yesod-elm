module App.Model where

import TransitRouter exposing (WithRoute)
import App.Router exposing (Route)
import Pages.Article.Model as Article exposing (initialModel, Model)

type alias Model = WithRoute Route { article : Article.Model }

initialModel : Model
initialModel = { transitRouter = TransitRouter.empty App.Router.EmptyRoute
               , article = Article.initialModel }
