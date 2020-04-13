module Model where

import           ClassyPrelude.Yesod

import           Database.Esqueleto

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
    deriving Eq Show Typeable
Product json
    title Text
    category CategoryId Maybe
    description Text Maybe
    image Text Maybe
    deriving Eq Show Typeable
Category json
    title Text
    description Text Maybe
    deriving Eq Show Typeable
|]
