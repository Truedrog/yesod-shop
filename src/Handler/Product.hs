{-# LANGUAGE TypeApplications #-}

module Handler.Product where
import Database.Esqueleto hiding (Value, from, on)
import Database.Esqueleto.Experimental
import Import hiding ((==.), isNothing, on, product)

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
  products <- runDB $ select $ do
    product <- from $ Table @Product
    where_ (product ^. ProductCategory ==. val categoryId)
    pure product
  pure $ object ["status" .= ("ok" :: Text), "result" .= products]

getProductsR :: Handler Value
getProductsR = do
  products <- runDB $ select $ from $ Table @Product
  pure $ object ["status" .= ("ok" :: Text), "result" .= products]

