module Handler.Product where

import Import hiding ((==.), isNothing, on)

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
    Just res ->
      pure $
        object
          [ "status" .= ("ok" :: Text),
            "result" .= res
          ]
