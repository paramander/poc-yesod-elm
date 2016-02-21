module Pages.Article.View where

import Pages.Article.Model as Article exposing (Model)
import Pages.Article.Update exposing (Action)

import ArticleForm.View as ArticleForm exposing (view)
import ArticleList.View as ArticleList exposing (view)

import Html exposing (..)
import Html.Attributes exposing (id, class)

view : Signal.Address Action -> Article.Model -> Html
view address model =
  let
    childArticleFormAddress =
      Signal.forwardTo address Pages.Article.Update.ChildArticleFormAction
    childArticleListAddress =
      Signal.forwardTo address Pages.Article.Update.ChildArticleListAction
  in
    div
      [ id "article-page"
      , class "container"
      ]
      [ ArticleForm.view childArticleFormAddress model.articleForm
      , ArticleList.view childArticleListAddress model.articleList
      ]
