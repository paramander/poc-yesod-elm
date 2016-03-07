module Admin.Router where

import Json.Decode as Json
import Html exposing (Attribute)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onWithOptions)
import Effects exposing (Effects)
import RouteParser exposing (..)
import TransitRouter

type Route
  = DashboardPage
  | NewArticlePage
  | ArticleListPage
  | EmptyRoute

routeParsers : List (Matcher Route)
routeParsers =
  [ static DashboardPage "/admin"
  , static ArticleListPage "/admin/articles"
  , static NewArticlePage "/admin/articles/new"
  ]

decode : String -> Route
decode path =
  RouteParser.match routeParsers path
    |> Maybe.withDefault EmptyRoute

encode : Route -> String
encode route =
  case route of
    DashboardPage -> "/"
    ArticleListPage -> "/articles"
    NewArticlePage -> "/articles/new"
    EmptyRoute -> ""

redirect : Route -> Effects ()
redirect route =
  encode route
    |> Signal.send TransitRouter.pushPathAddress
    |> Effects.task

linkAttrs : Route -> List Attribute
linkAttrs route =
  let
    path = encode route
  in
    [ href path
    , onWithOptions
        "click"
        { stopPropagation = True, preventDefault = True }
        Json.value
        (\_ -> Signal.message TransitRouter.pushPathAddress path)
    ]
