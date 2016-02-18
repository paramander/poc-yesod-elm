{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}

import Api.Page hiding (Handler)
import Control.Monad.Logger (runLoggingT)
import Database.Persist.Postgresql
import System.Environment (lookupEnv)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import Yesod
import Yesod.Core.Types (Logger)
import Yesod.Default.Config2 (makeYesodLogger)
import qualified Data.ByteString.Char8 as BS

data App = App
    { appConnPool :: ConnectionPool
    , appLogger :: Logger
    , getPageApi :: PageApi
    }

mkYesod "App" [parseRoutes|
/api/pages PageApiR PageApi getPageApi
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
instance PageSubApi App

getHomeR :: Texts -> Handler Html
getHomeR _ = defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = do
    port <- lookupEnv "PORT" >>= return . maybe 3000 read
    dbConnString <- lookupEnv "DATABASE_URL" >>= return . maybe "postgresql://localhost/poc-yesod-elm" BS.pack
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    let getPageApi = PageApi
        mkFoundation appConnPool = App {..}
        tmpFoundation = mkFoundation $ error "fake connection pool"
        logFunc = messageLoggerSource tmpFoundation appLogger
    pool <- flip runLoggingT logFunc $ createPostgresqlPool dbConnString 10
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
    warp port $ mkFoundation pool
