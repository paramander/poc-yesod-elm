module App.Update where

import Effects exposing (Effects, none)
import TransitRouter

import App.Model as App exposing (initialModel, Model)
import App.Router as Router

import Pages.Article.Update exposing (Action)

type alias Model = App.Model

type Action = ChildArticleAction Pages.Article.Update.Action
            | RouterAction (TransitRouter.Action Router.Route)
            | NoOp

init : String -> (Model, Effects Action)
init path =
  TransitRouter.init routerConfig path App.initialModel

actions : Signal Action
actions =
  Signal.map RouterAction TransitRouter.actions

routerConfig : TransitRouter.Config Router.Route Action Model
routerConfig = { mountRoute = mountRoute
               , getDurations = \_ _ _ -> (50, 200)
               , actionWrapper = RouterAction
               , routeDecoder = Router.decode
               }

mountRoute : Router.Route -> Router.Route -> Model -> (Model, Effects Action)
mountRoute prev route model =
  case route of
    Router.NewArticlePage ->
      (model, Effects.none)
    Router.ArticleListPage ->
      (model, Effects.none)
    Router.EmptyRoute ->
      (model, Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    ChildArticleAction act ->
      let
        (childModel, childEffects) = Pages.Article.Update.update act model.article
      in
        ( { model | article = childModel }
        , Effects.map ChildArticleAction childEffects
        )
    RouterAction routeAction ->
      TransitRouter.update routerConfig routeAction model
    NoOp ->
      (model, Effects.none)
