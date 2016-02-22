module Article.Decoder where

import Article.Model as Article exposing (Model)

import Json.Decode as JD exposing ((:=))
import String

decode : JD.Decoder Article.Model
decode =
  let
    number : JD.Decoder Int
    number = JD.oneOf [ JD.int, JD.customDecoder JD.string String.toInt]
  in
    JD.object3 Article.Model
        ("id" := number)
        ("title" := JD.string)
        ("content" := JD.string)
