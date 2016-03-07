module App.View where

import App.Model as App exposing (Model)
import App.Update exposing (Action)
import App.Router exposing (linkAttrs, Route)

import TransitRouter
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import PageNotFound.View exposing (view)

import ArticleList.View as ArticleList exposing (view)

view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ menuContent address model
    , mainContent address model
    ]

menuContent : Signal.Address Action -> Model -> Html
menuContent address model =
  header [ ]
         [ nav [ id "nav" ]
               [ ul [ id "menu" ]
                    [ li [ class "menu-item" ]
                         [ a (linkAttrs App.Router.ArticleListPage) [ text "Articles" ] ]
                    ]
               ]
         ]

mainContent : Signal.Address Action -> Model -> Html
mainContent address model =
  case TransitRouter.getRoute model of
    App.Router.ArticleListPage ->
      let
        childAddress = Signal.forwardTo address App.Update.ChildArticleListAction
      in
        div [ id "article-page"
            , class "container"
            ]
            [ ArticleList.view childAddress model.articleList ]
    App.Router.EmptyRoute ->
      div [] [ PageNotFound.View.view ]
