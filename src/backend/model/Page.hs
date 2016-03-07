module Model.Page () where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Types (Entity (..))
import Model.Persist (Page (..))

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
    parseJSON invalid = typeMismatch "Page" invalid
