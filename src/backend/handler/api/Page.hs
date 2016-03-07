module Handler.Api.Page
       ( getPagesR
       , postPagesR
       , getPageR
       , putPageR
       , deletePageR
       ) where

import Model (Page, PageId)
import Foundation
import Yesod

getPagesR :: Handler Value
getPagesR = do
    posts <- runDB $ selectList [] [] :: Handler [Entity Page]
    return $ object ["pages" .= posts]

postPagesR :: Handler Value
postPagesR = do
    page <- requireJsonBody :: Handler Page
    id <- runDB $ insert page
    return $ object ["page" .= (Entity id page)]

getPageR :: PageId -> Handler Value
getPageR id = do
    page <- runDB $ get404 id
    return $ object ["page" .= (Entity id page)]

putPageR :: PageId -> Handler Value
putPageR id = do
    runDB $ get404 id
    page <- requireJsonBody :: Handler Page
    runDB $ replace id page
    return $ object ["page" .= (Entity id page)]

deletePageR :: PageId -> Handler Value
deletePageR id = do
    runDB $ do
        get404 id
        delete id
    return $ object ["page" .= object ["id" .= (toPathPiece id)]]
