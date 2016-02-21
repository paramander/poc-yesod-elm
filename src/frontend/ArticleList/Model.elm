module ArticleList.Model where

import Http exposing (Error)

import Article.Model as Article exposing (Model)

type alias Id = Int

type Status = Init
            | Fetching
            | Fetched
            | HttpError Http.Error

type alias Model = { articles : List Article.Model
                   , status : Status
                   }

initialModel : Model
initialModel = { articles = []
               , status = Init
               }
