{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}

import Control.Monad.Logger (runLoggingT)
import Database.Persist.Postgresql
import System.Environment (lookupEnv)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import Yesod
import Yesod.Core.Types (Logger)
import Yesod.Default.Config2 (makeYesodLogger)
import Yesod.EmbeddedStatic
import Yesod.Form.Remote
import Lucid hiding (Html)
import Yesod.Lucid
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS

data App = App
    { appStatic :: EmbeddedStatic
    , appConnPool :: ConnectionPool
    , appLogger :: Logger
    }

mkEmbeddedStatic False "embeddedStatic" [embedDir "src/static"]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Page
    name Text
    html Text
    deriving Show
|]

instance ToJSON (Entity Page) where
    toJSON (Entity pid p) = object
        [ "id" .= pid
        , "title" .= pageName p
        , "content" .= pageHtml p
        ]

instance FromJSON Page where
    parseJSON (Object v) = Page
        <$> v .: "title"
        <*> v .: "content"

mkYesod "App" [parseRoutes|
/static StaticR EmbeddedStatic appStatic
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
instance RenderMessage App FormMessage where
    renderMessage _ _ (MsgInputNotFound _) = "REQUIRED"
    renderMessage _ _ msg = defaultFormMessage msg

getHomeR :: Texts -> Handler LucidHtml
getHomeR _ = lucid $ \url -> do
    doctype_
    html_ [lang_ "nl"] $ do
        head_ $ do
            title_ "POC Yesod + Elm"
            meta_ [name_ "viewport", content_ "width=device-width, height=device-height, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0"]
            meta_ [charset_ "utf-8"]
            script_ [type_ "text/javascript", src_ . url $ StaticR app_js] ("" :: Text)
        body_ $ do
            script_ [type_ "text/javascript"] ("Elm.fullscreen(Elm.Main, {initialPath: window.location.pathname});" :: Text)

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

main :: IO ()
main = do
    port <- lookupEnv "PORT" >>= return . maybe 3000 read
    dbConnString <- lookupEnv "DATABASE_URL" >>= return . maybe "postgresql://localhost/poc-yesod-elm" BS.pack
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    let appStatic = embeddedStatic
        mkFoundation appConnPool = App {..}
        tmpFoundation = mkFoundation $ error "fake connection pool"
        logFunc = messageLoggerSource tmpFoundation appLogger
    pool <- flip runLoggingT logFunc $ createPostgresqlPool dbConnString 10
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
    warp port $ mkFoundation pool
