module Model where

import           ClassyPrelude.Yesod
import           Database.Esqueleto
import qualified Database.Esqueleto.PostgreSQL.JSON as PJSON
import Db.DbTypes

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    email Text
    password Text Maybe -- Password may not be set yet
    verkey Text Maybe -- Used for resetting passwords
    verified Bool
    UniqueUser email
    firstName Text Maybe
    lastName Text Maybe
    phone Text Maybe
    deriving Eq Show Typeable
Product json
    title Text
    category CategoryId
    description Text Maybe
    price Double
    badges (PJSON.JSONB [Text]) Maybe
    images (PJSON.JSONB [Text] ) Maybe
    rating Int
    availability Bool
    features (PJSON.JSONB [ProductFeature]) Maybe
    options (PJSON.JSONB [Text] ) Maybe
    spec (PJSON.JSONB [ProductSpecType]) Maybe
    deriving Eq Show Typeable
Category json
    title Text
    parentId CategoryId Maybe
    deriving Eq Show Typeable
|]
