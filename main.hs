{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}

import Yesod
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool)
import Database.Persist.Postgresql
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import Control.Monad.Logger (runLoggingT)
import Yesod.Core.Types (Logger)
import Yesod.Default.Config2 (makeYesodLogger)
import Data.Text (Text)

data App = App
    { appConnPool :: ConnectionPool
    , appLogger :: Logger
    }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Page
    name Text
    html Text
    deriving Show
|]

instance ToJSON (Entity Page) where
    toJSON (Entity pid p) = object
        [ "id" .= (toPathPiece pid)
        , "title" .= pageName p
        , "body" .= pageHtml p
        ]

mkYesod "App" [parseRoutes|
/api/pages PagesR GET POST
/api/pages/#PageId PageR GET PUT DELETE
!/+Texts HomeR GET
|]

instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

getHomeR :: Texts -> Handler Html
getHomeR _ = defaultLayout [whamlet|Hello World!|]

getPagesR :: Handler Value
getPagesR = do
    posts <- runDB $ selectList [] [] :: Handler [Entity Page]
    return $ object ["pages" .= posts]

postPagesR :: Handler Value
postPagesR = do
    -- TODO: (Mats Rietdijk) use form
    let page = Page "title" "<p>created</p>"
    id <- runDB $ insert page
    return $ object ["page" .= (Entity id page)]

getPageR :: PageId -> Handler Value
getPageR id = do
    page <- runDB $ get404 id
    return $ object ["page" .= (Entity id page)]

putPageR :: PageId -> Handler Value
putPageR id = do
    -- TODO: (Mats Rietdijk) use form
    let page = Page "title" "<p>updated</p>"
    runDB $ do
        get404 id
        replace id page
    return $ object ["page" .= (Entity id page)]

deletePageR :: PageId -> Handler Value
deletePageR id = do
    runDB $ do
        get404 id
        delete id
    return $ object ["page" .= object ["id" .= (toPathPiece id)]]

main :: IO ()
main = do
    -- TODO: (Mats Rietdijk) use env for PORT & DATABASE_URL
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    let mkFoundation appConnPool = App {..}
        tmpFoundation = mkFoundation $ error "fake connection pool"
        logFunc = messageLoggerSource tmpFoundation appLogger
    pool <- flip runLoggingT logFunc $ createPostgresqlPool "" 10
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
    warp 3000 $ mkFoundation pool
