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
      , lazy2 nameInput address model.articleForm.name
      , lazy2 bodyInput address model.articleForm.body
      , button
          [ onClick address ArticleForm.Update.SubmitForm
          , class "btn btn-primary"
          , disabled (String.isEmpty model.articleForm.name)
          ]
          [ text "Submit" ]
      ]

nameInput : Signal.Address Action -> String -> Html
nameInput address title =
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
        , on "input" targetValue (Signal.message address << ArticleForm.Update.UpdateName)
        ]
        []
    ]

bodyInput : Signal.Address Action -> String -> Html
bodyInput address body =
  div
    [ class "input" ]
    [
      label
         [ for "article-body" ]
         [ text "content" ]
    , textarea
        [ id "article-body"
        , value body
        , name "body"
        , on "input" targetValue (Signal.message address << ArticleForm.Update.UpdateBody)
        ]
        [ text body ]
    ]

sluggify : String -> String
sluggify s =
  replace Regex.All (regex "\\W") (\_ -> "") (String.toLower s)
