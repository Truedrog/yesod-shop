module Handler.Product where

import Import

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
