module App.View where

import App.Model as App exposing (initialModel, Model)
import App.Update exposing (init, Action)
import App.Router exposing (Route)

import TransitRouter
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Pages.PageNotFound.View exposing (view)

import ArticleForm.View as ArticleForm exposing (view)
import ArticleList.View as ArticleList exposing (view)

view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ mainContent address model ]

mainContent : Signal.Address Action -> Model -> Html
mainContent address model =
  case TransitRouter.getRoute model of
    App.Router.NewArticlePage ->
      let
        childAddress = Signal.forwardTo address App.Update.ChildArticleFormAction
      in
        div [ id "article-page"
            , class "container"
            ]
            [ ArticleForm.view childAddress model.articleForm ]
    App.Router.ArticleListPage ->
      let
        childAddress = Signal.forwardTo address App.Update.ChildArticleListAction
      in
        div [ id "article-page"
            , class "container"
            ]
            [ ArticleList.view childAddress model.articleList ]
    App.Router.EmptyRoute ->
      div [] [ Pages.PageNotFound.View.view ]
