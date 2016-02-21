module App.Router where

import Json.Decode as Json
import Html exposing (Attribute)
import Html.Events exposing (on)
import Effects exposing (Effects)
import RouteParser exposing (..)
import TransitRouter

type Route = NewArticlePage
           | ArticleListPage
           | EmptyRoute

routeParsers : List (Matcher Route)
routeParsers =
  [ static ArticleListPage "/"
  , static NewArticlePage "/article/new"
  ]

decode : String -> Route
decode path =
  RouteParser.match routeParsers path
    |> Maybe.withDefault EmptyRoute

encode : Route -> String
encode route =
  case route of
    ArticleListPage -> "/"
    NewArticlePage -> "/article/new"
    EmptyRoute -> ""

redirect : Route -> Effects ()
redirect route =
  encode route
    |> Signal.send TransitRouter.pushPathAddress
    |> Effects.task

clickAttr : Route -> Attribute
clickAttr route =
  on "click" Json.value (\_ -> Signal.message TransitRouter.pushPathAddress <| encode route)
