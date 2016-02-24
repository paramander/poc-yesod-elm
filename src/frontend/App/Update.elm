module App.Update where

import Effects exposing (Effects, none)
import Task exposing (succeed)
import TransitRouter

import App.Model as App exposing (initialModel, Model)
import App.Router as Router

import ArticleForm.Model exposing (initialModel)
import ArticleForm.Update exposing (Action)
import ArticleList.Update exposing (Action)

type alias Model = App.Model

type Action = ChildArticleListAction ArticleList.Update.Action
            | ChildArticleFormAction ArticleForm.Update.Action
            | RouterAction (TransitRouter.Action Router.Route)
            | NoOp

init : String -> (Model, Effects Action)
init path =
  TransitRouter.init routerConfig path (App.initialModel (Router.decode path))

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
      ( { model | articleForm = ArticleForm.Model.initialModel }
      , Effects.none
      )
    Router.ArticleListPage ->
      ( model
      , Task.succeed (ChildArticleListAction ArticleList.Update.GetData) |> Effects.task
      )
    Router.EmptyRoute ->
      (model, Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    ChildArticleListAction act ->
      let
        (childModel, childEffects) = ArticleList.Update.update act model.articleList
      in
        ( { model | articleList = childModel }
        , Effects.map ChildArticleListAction childEffects
        )
    ChildArticleFormAction act ->
      let
        (childModel, childEffects, maybeArticle) = ArticleForm.Update.update act model.articleForm
      in
        ( { model | articleForm = childModel }
        , Effects.map ChildArticleFormAction childEffects
        )
    RouterAction routeAction ->
      TransitRouter.update routerConfig routeAction model
    NoOp ->
      (model, Effects.none)
