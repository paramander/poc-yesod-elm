module Article.Model where

type alias Id = Int

type alias Model = { id : Id
                   , title : String
                   , content : String
                   }
