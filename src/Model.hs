module Model where

import ClassyPrelude.Yesod
import Database.Esqueleto
import qualified Database.Esqueleto.PostgreSQL.JSON as PJSON
import Database.Persist.Quasi
import Db.DbTypes

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")
