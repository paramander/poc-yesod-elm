module ArticleForm.Model where

type PostStatus = Busy
                | Done
                | Ready

type alias ArticleForm = { name : String
                         , body : String
                         , show : Bool
                         }

type UserMessage = None
                 | Error String

type alias Model = { articleForm : ArticleForm
                   , postStatus : PostStatus
                   , userMessage : UserMessage
                   }

initialArticleForm : ArticleForm
initialArticleForm = { name = ""
                     , body = ""
                     , show = True
                     }

initialModel : Model
initialModel = { articleForm = initialArticleForm
               , postStatus = Ready
               , userMessage = None
               }
