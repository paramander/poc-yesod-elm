module Admin.Update where

import Effects exposing (Effects, none)
import Task exposing (succeed)
import TransitRouter

import Admin.Model as Admin exposing (initialModel, Model)
import Admin.Router as Router

import ArticleForm.Model exposing (initialModel)
import ArticleForm.Update exposing (Action)
import ArticleList.Update exposing (Action)
import AdminDashboard.Update exposing (Action)

type alias Model = Admin.Model

type Action
  = RouterAction (TransitRouter.Action Router.Route)
  | ChildDashboardAction AdminDashboard.Update.Action
  | ChildArticleListAction ArticleList.Update.Action
  | ChildArticleFormAction ArticleForm.Update.Action
  | NoOp

init : String -> (Model, Effects Action)
init path =
  TransitRouter.init routerConfig path (Admin.initialModel (Router.decode path))

actions : Signal Action
actions =
  Signal.map RouterAction TransitRouter.actions

routerConfig : TransitRouter.Config Router.Route Action Model
routerConfig =
  { mountRoute = mountRoute
  , getDurations = \_ _ _ -> (50, 200)
  , actionWrapper = RouterAction
  , routeDecoder = Router.decode
  }

mountRoute : Router.Route -> Router.Route -> Model -> (Model, Effects Action)
mountRoute prev route model =
  case route of
    Router.DashboardPage ->
      ( model
      , Effects.none
      )
    Router.ArticleListPage ->
      ( model
      , Task.succeed (ChildArticleListAction ArticleList.Update.GetData) |> Effects.task
      )
    Router.NewArticlePage ->
      ( { model | articleForm = ArticleForm.Model.initialModel }
      , Effects.none
      )
    Router.EmptyRoute ->
      ( model
      , Effects.none
      )

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    ChildDashboardAction act ->
      let
        (childModel, childEffects) = AdminDashboard.Update.update act model.dashboard
      in
        ( { model | dashboard = childModel }
        , Effects.map ChildDashboardAction childEffects
        )
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
