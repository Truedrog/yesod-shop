module Handler.Data where

import Database.Esqueleto hiding (from, on)
import Database.Esqueleto.Experimental
import Import hiding ((==.), (||.))
import Db.DbStuff

getCatsR :: Handler Import.Value
getCatsR = do
--  _    <- runDB $ setupDb
  cats <- runDB $ statement
  products <- runDB $ selectProducts
  returnJson (cats, products)
  where
    selectProducts = select $ from $ Table @Product
    statement = select do
                    cat <- from $ Table @Category
                    where_ (cat ^. CategoryParentId `in_` valList [Just (toSqlKey 1), Just (toSqlKey 2)])
                    pure cat
