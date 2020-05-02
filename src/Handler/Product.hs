{-# LANGUAGE TypeApplications #-}

module Handler.Product where

import Database.Esqueleto hiding (Value, from, on)
import Database.Esqueleto.Experimental
import Import hiding ((==.), isNothing, on, product, (||.))

getProductR :: ProductId -> Handler Value
getProductR productId = do
  maybeProduct <- runDB $ get productId
  case maybeProduct of
    Nothing ->
      pure $
        object
          [ "status" .= ("error" :: Text),
            "description" .= ("not found" :: Text)
          ]
    Just p -> do
      let catId = productCategory p
      cat <- runDB $ get catId
      pure $
        object
          [ "status" .= ("ok" :: Text),
            "result" .= object ["product" .= p, "category" .= cat]
          ]

getProductsByCatR :: CategoryId -> Handler Value
getProductsByCatR categoryId = do
  products <- runDB $ selectProductsByCat $ categoryId
  pure $ object ["status" .= ("ok" :: Text), "result" .= products]

selectProductsByCat :: CategoryId -> DB [Entity Product]
selectProductsByCat categoryId =
  select $ do
    (product :& cat :& root) <-
      from $
        Table @Product
          `InnerJoin` Table @Category
          `on` (\(product :& sub) -> sub ^. CategoryId ==. product ^. ProductCategory)
          `LeftOuterJoin` Table @Category
          `on` (\(_ :& sub :& root) -> root ?. CategoryId ==. sub ^. CategoryParentId)
    where_ 
      $ cat ^. CategoryId ==. val categoryId
      ||. root ?. CategoryId ==. val (Just categoryId)
    pure product

getProductsR :: Handler Value
getProductsR = do
  products <- runDB $ select $ from $ Table @Product
  pure $ object ["status" .= ("ok" :: Text), "result" .= products]
