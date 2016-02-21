module ArticleList.View where

import Article.Model exposing (Model)

import ArticleList.Model exposing (initialModel, Model)
import ArticleList.Update exposing (Action)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import String

type alias Article = Article.Model.Model
type alias Model = ArticleList.Model.Model

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "wrapper" ]
    [ h3
        [ class "title" ]
        [ i [ class "fa fa-file-o icon" ] []
        , text "Recent articles"
        ]
    , ul
        [ id "page-list"
        , class "list"
        ]
        (List.map viewArticle model.articles)
    ]

viewArticle : Article -> Html
viewArticle article =
  li
    [ class "page-item" ]
      [ p [] [ span [ class "title" ] [ text article.name ]
             , span [ class "exerpt"] [ text (exerpt 10 article) ]
             ]
      ]

exerpt : Int -> Article -> String
exerpt length article =
  if String.length article.body > length then
    String.left length (String.trim article.body) ++ "..."
  else
    article.body
