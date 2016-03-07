module Admin.View where

import Admin.Model as Admin exposing (Model)
import Admin.Update exposing (Action)
import Admin.Router exposing (linkAttrs, Route)

import TransitRouter
import Html exposing (..)
import Html.Attributes exposing (..)

import PageNotFound.View exposing (view)

import AdminDashboard.View as AdminDashboard exposing (view)
import ArticleList.View as ArticleList exposing (view)
import ArticleForm.View as ArticleForm exposing (view)

view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ menuContent address model
    , mainContent address model
    ]

menuContent : Signal.Address Action -> Model -> Html
menuContent address model =
  header
    []
    [ nav [ id "nav" ]
          [ ul [ id "menu" ]
              [ li [ class "menu-item" ]
                   [ a (linkAttrs Admin.Router.ArticleListPage) [ text "Articles" ] ]
              , li [ class "menu-item" ]
                   [ a (linkAttrs Admin.Router.NewArticlePage) [ text "Create article" ] ]
              ]
          ]
    ]

mainContent : Signal.Address Action -> Model -> Html
mainContent address model =
  case TransitRouter.getRoute model of
    Admin.Router.DashboardPage ->
      let
        childAddress = Signal.forwardTo address Admin.Update.ChildDashboardAction
      in
        div [ id "dashboard"
            , class "container"
            ]
            [ AdminDashboard.view childAddress model.dashboard ]
    Admin.Router.ArticleListPage ->
      let
        childAddress = Signal.forwardTo address Admin.Update.ChildArticleListAction
      in
        div [ id "article-list"
            , class "container"
            ]
            [ ArticleList.view childAddress model.articleList ]
    Admin.Router.NewArticlePage ->
      let
        childAddress = Signal.forwardTo address Admin.Update.ChildArticleFormAction
      in
        div [ id "article-form"
            , class "container"
            ]
            [ ArticleForm.view childAddress model.articleForm ]
    Admin.Router.EmptyRoute ->
      div [] [ PageNotFound.View.view ]
