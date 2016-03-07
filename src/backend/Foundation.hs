module Foundation where

import Database.Persist.Postgresql
import Yesod
import Yesod.Core.Types (Logger)
import Yesod.EmbeddedStatic
import Model

data App = App
    { appStatic :: EmbeddedStatic
    , appConnPool :: ConnectionPool
    , appLogger :: Logger
    }

mkYesodData "App" [parseRoutes|
/static StaticR EmbeddedStatic appStatic
/api/pages PagesR GET POST
/api/pages/#PageId PageR GET PUT DELETE
/admin/+Texts AdminR GET
!/+Texts HomeR GET
|]

#if DEVELOPMENT
mkEmbeddedStatic True "embeddedStatic" [embedDir "src/static"]
#else
mkEmbeddedStatic False "embeddedStatic" [embedDir "src/static"]
#endif

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
