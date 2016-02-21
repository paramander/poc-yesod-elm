module ArticleForm.Update where

import Effects exposing (Effects)
import Http exposing (post, Error)
import Json.Decode as JD
import Json.Encode as JE exposing (string)
import Task exposing (Task)

import Article.Model as Article exposing (Model)
import Article.Decoder
import ArticleForm.Model as ArticleForm exposing (initialArticleForm, initialModel, ArticleForm, Model, UserMessage)

init : (ArticleForm.Model, Effects Action)
init = ( initialModel
       , Effects.none
       )

type Action = ResetForm
            | SubmitForm
            | UpdateContent String
            | UpdateTitle String
            | UpdatePostArticle (Result Http.Error Article.Model)

type alias Model = ArticleForm.Model

update : Action -> Model -> (Model, Effects Action, Maybe Article.Model)
update action model =
  case action of
    ResetForm ->
      ( { model | articleForm = initialArticleForm
                , postStatus = ArticleForm.Ready
        }
      , Effects.none
      , Nothing
      )
    SubmitForm ->
      let url = "http://localhost:3000/api/pages"
      in
        if model.postStatus == ArticleForm.Ready
          then
            ( { model | postStatus = ArticleForm.Busy }
            , postArticle url model.articleForm
            , Nothing
            )
          else
            ( model
            , Effects.none
            , Nothing
            )
    UpdatePostArticle result ->
      case result of
        Ok article ->
          ( { model | postStatus = ArticleForm.Done }
          , Task.succeed ResetForm |> Effects.task
          , Just article
          )
        Err err ->
          ( model
          , Effects.none
          , Nothing
          )
    UpdateContent val ->
      let
        articleForm = model.articleForm
        articleForm' = { articleForm | content = val }
      in
        ( { model | articleForm = articleForm' }
        , Effects.none
        , Nothing
        )

    UpdateTitle val ->
      let
        articleForm = model.articleForm
        articleForm' = { articleForm | title = val }
      in
        ( { model | articleForm = articleForm' }
        , Effects.none
        , Nothing
        )

postArticle : String -> ArticleForm.ArticleForm -> Effects Action
postArticle url data =
  Http.post
      decodePostArticle
      url
      (Http.string <| dataToJson data)
      |> Task.toResult
      |> Task.map UpdatePostArticle
      |> Effects.task

dataToJson : ArticleForm.ArticleForm -> String
dataToJson data =
  JE.encode 0
    <| JE.object
       [ ("title", JE.string data.title)
       , ("content", JE.string data.content)
       ]

decodePostArticle : JD.Decoder Article.Model
decodePostArticle =
  JD.at ["page"] <| Article.Decoder.decode
