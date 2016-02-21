module Article.Model where

type alias Id = Int

type alias Model = { id : Id
                   , name : String
                   , body : String
                   }
