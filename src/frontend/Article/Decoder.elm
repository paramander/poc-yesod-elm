module Article.Decoder where

import Article.Model as Article exposing (Model)

import Json.Decode as JD exposing ((:=))

decode : JD.Decoder Article.Model
decode =
  JD.object3 Article.Model
    ("id" := JD.int)
    ("title" := JD.string)
    ("body" := JD.string)
