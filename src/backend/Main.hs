{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}

import Control.Monad.Logger (runLoggingT)
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool)
import Network.URI
import System.Environment (getEnv, lookupEnv)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import Yesod
import Yesod.Core.Types (Logger)
import Yesod.Default.Config2 (makeYesodLogger)
import Yesod.Form.Remote
import qualified Data.Text as T

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
instance RenderMessage App FormMessage where
    renderMessage _ _ (MsgInputNotFound _) = "REQUIRED"
    renderMessage _ _ msg = defaultFormMessage msg

getHomeR :: Texts -> Handler Html
getHomeR _ = defaultLayout [whamlet|Hello World!|]

getPagesR :: Handler Value
getPagesR = do
    posts <- runDB $ selectList [] [] :: Handler [Entity Page]
    return $ object ["pages" .= posts]

postPagesR :: Handler Value
postPagesR = do
    runPageForm insertPage
    where
        insertPage page = do
            id <- runDB $ insert page
            return $ object ["page" .= (Entity id page)]

getPageR :: PageId -> Handler Value
getPageR id = do
    page <- runDB $ get404 id
    return $ object ["page" .= (Entity id page)]

putPageR :: PageId -> Handler Value
putPageR id = do
    runDB $ get404 id
    runPageForm updatePage
    where
        updatePage page = do
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
    dbConnString <- getEnv "DATABASE_URL" >>= return . toConnectionString
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    let mkFoundation appConnPool = App {..}
        tmpFoundation = mkFoundation $ error "fake connection pool"
        logFunc = messageLoggerSource tmpFoundation appLogger
    pool <- flip runLoggingT logFunc $ createPostgresqlPool dbConnString 10
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
    warp port $ mkFoundation pool

runPageForm :: (Page -> Handler Value) -> Handler Value
runPageForm f = do
    result <- runRemotePost $ Page
        <$> rreq textField "title"
        <*> rreq textField "content"
    case result of
        RemoteFormSuccess page ->
            f page
        RemoteFormFailure errors ->
            return . object $ map (uncurry (.=)) errors

toConnectionString :: String -> ConnectionString
toConnectionString s = case parseURI s of
    Nothing -> error "Invalid database url provided"
    Just uri -> do
        let uaps = case uriAuthority uri of
                Nothing ->
                    []
                Just auth ->
                    [ ("dbuser", fst up)
                    , ("password", T.drop 1 $ snd up)
                    , ("host", pack $ uriRegName auth)
                    , ("port", T.drop 1 . pack $ uriPort auth)
                    ]
                    where
                        up :: (Text, Text)
                        up = T.breakOn ":" . T.dropEnd 1 . pack $ uriUserInfo auth
        encodeUtf8 . joinParts $ uaps ++ [("dbname", T.drop 1 . T.pack $ uriPath uri)]
    where
        joinParts :: [(Text, Text)] -> Text
        joinParts = T.unwords . map (\(a, b) -> a <> "=" <> b) . filter (not . T.null . snd)
