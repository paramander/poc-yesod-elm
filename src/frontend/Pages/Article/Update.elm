module Pages.Article.Update where

import Effects exposing (Effects)
import Task exposing (succeed)

import ArticleForm.Update exposing (Action)
import ArticleList.Update exposing (Action)

import Pages.Article.Model exposing (Model)

type Action = Activate
            | ChildArticleFormAction ArticleForm.Update.Action
            | ChildArticleListAction ArticleList.Update.Action

init : (Model, Effects Action)
init = ( Pages.Article.Model.initialModel
       , Effects.none
       )

update : Action -> Pages.Article.Model.Model -> (Pages.Article.Model.Model, Effects Action)
update action model =
  case action of
    Activate ->
      ( model
      , Task.succeed (ChildArticleListAction ArticleList.Update.GetData) |> Effects.task
      )
    ChildArticleFormAction act ->
      let (childModel, childEffects, maybeArticle) = ArticleForm.Update.update act model.articleForm
      in ( { model | articleForm = childModel }
         , Effects.map ChildArticleFormAction childEffects
         )
    ChildArticleListAction act ->
      let (childModel, childEffects) = ArticleList.Update.update act model.articleList
      in ( { model | articleList = childModel }
         , Effects.map ChildArticleListAction childEffects
         )
