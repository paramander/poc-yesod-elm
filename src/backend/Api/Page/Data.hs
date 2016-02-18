{-# LANGUAGE RankNTypes #-}

module Api.Page.Data where

import Yesod
import Database.Persist.Postgresql
import Data.Text (Text)

data PageApi = PageApi

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Page
    name Text
    html Text
    deriving Show
|]

mkYesodSubData "PageApi" [parseRoutes|
/ PagesR GET POST
/#PageId PageR GET PUT DELETE
|]

instance ToJSON (Entity Page) where
    toJSON (Entity pid p) = object
        [ "id" .= (toPathPiece pid)
        , "title" .= pageName p
        , "body" .= pageHtml p
        ]

type PageHandler a = forall master.
    ( Yesod master
    , YesodPersist master
    , YesodPersistBackend master ~ SqlBackend
    , RenderMessage master FormMessage
    ) => HandlerT PageApi (HandlerT master IO) a
