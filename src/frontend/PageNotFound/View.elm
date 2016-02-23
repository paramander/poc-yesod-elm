module PageNotFound.View where

import App.Router exposing (linkAttrs, Route)

import Html exposing (a, i, div, h2, text, Html)
import Html.Attributes exposing (class, id, href)

view : Html
view =
  div
    [ id "page-not-found"
    , class "container"
    ]
    [ div
        [ class "wrapper text-center" ]
        [ h2 [] [ text "This is a 404 page!" ]
        , a (linkAttrs App.Router.ArticleListPage) [ text "Back to safety" ]
        ]
    ]
