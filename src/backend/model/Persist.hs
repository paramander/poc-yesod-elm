{-# LANGUAGE TemplateHaskell #-}

module Model.Persist
       ( Page (..)
       , PageId
       , migrateAll
       ) where

import Database.Persist.TH
import Data.Text (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Page
    name Text
    html Text
    deriving Show
|]
