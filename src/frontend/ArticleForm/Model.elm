module ArticleForm.Model where

type PostStatus = Busy
                | Done
                | Ready

type alias ArticleForm = { title : String
                         , content : String
                         , show : Bool
                         }

type UserMessage = None
                 | Error String

type alias Model = { articleForm : ArticleForm
                   , postStatus : PostStatus
                   , userMessage : UserMessage
                   }

initialArticleForm : ArticleForm
initialArticleForm = { title = ""
                     , content = ""
                     , show = True
                     }

initialModel : Model
initialModel = { articleForm = initialArticleForm
               , postStatus = Ready
               , userMessage = None
               }
