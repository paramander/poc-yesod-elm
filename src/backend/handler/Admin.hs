module Handler.Admin
       ( getAdminR
       ) where

import Data.Text (Text)
import Foundation
import Lucid hiding (Html)
import Yesod.Lucid
import Yesod

getAdminR :: Texts -> Handler LucidHtml
getAdminR _ = lucid $ \url -> do
    doctype_
    html_ [lang_ "nl"] $ do
        head_ $ do
            title_ "POC Yesod + Elm | Admin"
            meta_ [name_ "viewport", content_ "width=device-width, height=device-height, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0"]
            meta_ [charset_ "utf-8"]
            script_ [type_ "text/javascript", src_ . url $ StaticR admin_js] ("" :: Text)
        body_ $ do
            script_ [type_ "text/javascript"] ("Elm.fullscreen(Elm.Main, {initialPath: window.location.pathname});" :: Text)
