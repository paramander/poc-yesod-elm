{-# LANGUAGE RankNTypes #-}

module Api.Page
       ( module Api.Page.Data
       , module Api.Page
       ) where

import Api.Page.Data
import Database.Persist.Postgresql
import Yesod
import Yesod.Form.Remote

instance ( Yesod master
         , YesodPersist master
         , YesodPersistBackend master ~ SqlBackend
         , RenderMessage master FormMessage
         ) => YesodSubDispatch PageApi (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesPageApi)

getPagesR :: PageHandler Value
getPagesR = do
    posts <- lift . runDB $ selectList [] [] :: PageHandler [Entity Page]
    return $ object ["pages" .= posts]

postPagesR :: PageHandler Value
postPagesR = lift $ do
    runPageForm insertPage
    where
        insertPage page = do
            id <- runDB $ insert page
            return $ object ["page" .= (Entity id page)]

getPageR :: PageId -> PageHandler Value
getPageR id = lift $ do
    page <- runDB $ get404 id
    return $ object ["page" .= (Entity id page)]

putPageR :: PageId -> PageHandler Value
putPageR id = lift $ do
    runDB $ get404 id
    runPageForm updatePage
    where
        updatePage page = do
            runDB $ replace id page
            return $ object ["page" .= (Entity id page)]

deletePageR :: PageId -> PageHandler Value
deletePageR id = lift $ do
    runDB $ do
        get404 id
        delete id
    return $ object ["page" .= object ["id" .= (toPathPiece id)]]

runPageForm f = do
    result <- runRemotePost $ Page
        <$> rreq textField "title"
        <*> rreq textField "content"
    case result of
        RemoteFormSuccess page ->
            f page
        RemoteFormFailure errors ->
            return . object $ map (uncurry (.=)) errors
