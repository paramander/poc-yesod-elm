module ArticleForm.View where

import ArticleForm.Model exposing (initialModel, Model, UserMessage)
import ArticleForm.Update exposing (Action)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Html.Events exposing (..)
import Regex exposing (replace, regex)
import String

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "wrapper" ]
    [ viewUserMessage model.userMessage
    , viewForm address model
    ]

viewUserMessage : UserMessage -> Html
viewUserMessage userMessage =
  case userMessage of
    ArticleForm.Model.None ->
      div [] []
    ArticleForm.Model.Error message ->
      div [ style [("text-align", "center")] ] [ text message ]

viewForm : Signal.Address Action -> Model -> Html
viewForm address model =
  Html.form
      [ onSubmit address ArticleForm.Update.SubmitForm
      , action "javascript:void(0);"
      ]
      [ h3
        [ class "name" ]
        [ i [ class "fa fa-pencil" ] []
        , text "Add new article"
        ]
      , lazy2 titleInput address model.articleForm.title
      , lazy2 contentInput address model.articleForm.content
      , button
          [ onClick address ArticleForm.Update.SubmitForm
          , class "btn btn-primary"
          , disabled (String.isEmpty model.articleForm.title)
          ]
          [ text "Submit" ]
      ]

titleInput : Signal.Address Action -> String -> Html
titleInput address title =
  div
    [ class "input" ]
    [
      label
        [ for "article-title" ]
        [ text "title" ]
    , input
        [ id "article-title"
        , placeholder "Title"
        , value title
        , name "title"
        , on "input" targetValue (Signal.message address << ArticleForm.Update.UpdateTitle)
        ]
        []
    ]

contentInput : Signal.Address Action -> String -> Html
contentInput address content =
  div
    [ class "input" ]
    [
      label
         [ for "article-body" ]
         [ text "content" ]
    , textarea
        [ id "article-body"
        , value content
        , name "content"
        , on "input" targetValue (Signal.message address << ArticleForm.Update.UpdateContent)
        ]
        [ text content ]
    ]

sluggify : String -> String
sluggify s =
  replace Regex.All (regex "\\W") (\_ -> "") (String.toLower s)
