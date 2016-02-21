module ArticleList.Update where

import Effects exposing (Effects)
import Task exposing (andThen, Task)
import Http exposing (Error)
import Json.Decode as JD

import Article.Model as Article exposing (Model)
import Article.Decoder exposing (decode)
import ArticleList.Model exposing (initialModel, Model)

init : (ArticleList.Model.Model, Effects Action)
init = ( initialModel
       , Effects.none
       )

type Action = AppendArticle Article.Model
            | GetData
            | NoOp
            | UpdateDataFromServer (Result Http.Error (List Article.Model))

update : Action -> ArticleList.Model.Model -> (ArticleList.Model.Model, Effects Action)
update action model =
  case action of
    AppendArticle article ->
      ( { model | articles = article :: model.articles }
      , Effects.none
      )
    GetData ->
      let
        url = "http://localhost:3000/api/pages"
        effects =
          case model.status of
            ArticleList.Model.Fetching ->
              Effects.none
            _ ->
              getJson url
      in ( { model | status = ArticleList.Model.Fetching }
         , effects
         )
    NoOp ->
      ( model
      , Effects.none
      )
    UpdateDataFromServer result ->
      case result of
        Ok articles ->
          ( { model | articles = articles
                    , status = ArticleList.Model.Fetched
            }
          , Effects.none
          )
        Err err ->
          ( { model | status = ArticleList.Model.HttpError err }
          , Effects.none
          )

getJson : String -> Effects Action
getJson url =
  let
    httpTask = Task.toResult <| Http.get decodeData url
    actionTask = httpTask `andThen`
                 (\result -> Task.succeed (UpdateDataFromServer result))
  in
    Effects.task actionTask

decodeData : JD.Decoder (List Article.Model)
decodeData = JD.at ["pages"] <| JD.list <| Article.Decoder.decode
