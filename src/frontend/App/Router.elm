module App.Router where

import Json.Decode as Json
import Html exposing (Attribute)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onWithOptions)
import Effects exposing (Effects)
import RouteParser exposing (..)
import TransitRouter

type Route
  =  ArticleListPage
  | EmptyRoute

routeParsers : List (Matcher Route)
routeParsers =
  [ static ArticleListPage "/" ]

decode : String -> Route
decode path =
  RouteParser.match routeParsers path
    |> Maybe.withDefault EmptyRoute

encode : Route -> String
encode route =
  case route of
    ArticleListPage -> "/"
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

clickAttr : Route -> Attribute
clickAttr route =
  on "click" Json.value (\_ -> Signal.message TransitRouter.pushPathAddress <| encode route)
