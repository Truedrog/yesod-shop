module Model where

import ClassyPrelude.Yesod
    ( Eq,
      Show,
      Typeable,
      Bool,
      Double,
      Int,
      Text,
      mkMigrate,
      mkPersist,
      persistFileWith,
      share,
      sqlSettings )
import Database.Esqueleto ( BackendKey(SqlBackendKey) )
import qualified Database.Esqueleto.PostgreSQL.JSON as PJSON
import Database.Persist.Quasi ( lowerCaseSettings )
import Db.DbTypes ( ProductFeature, ProductSpecType )

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")
