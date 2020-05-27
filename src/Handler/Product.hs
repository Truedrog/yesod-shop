{-# LANGUAGE TypeApplications #-}

module Handler.Product where

import Data.Either (fromRight)
import Data.Text.Read (decimal)
import Database.Esqueleto hiding (Value, from, on)
import Database.Esqueleto.Experimental
import Import hiding ((==.), count, isNothing, on, product, (||.))

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

params :: MonadHandler m => m (Int64, Int64)
params = do
  ps <- reqGetParams <$> getRequest
  let (o, _) = fromRight (0, "_") (decimal . fromMaybe "0" $ lookup "offset" ps :: Either String (Int64, Text))
  let (l, _) = fromRight (12, "_") (decimal . fromMaybe "12" $ lookup "limit" ps :: Either String (Int64, Text))
  pure (o, l)

getProductsByCatR :: CategoryId -> Handler Value
getProductsByCatR categoryId = do
  (o, l) <- params
  products <- runDB $ selectProductsByCat categoryId l o
  pure $ object ["status" .= ("ok" :: Text), "result" .= products]

selectProductsByCat :: CategoryId -> Int64 -> Int64 -> DB [Entity Product]
selectProductsByCat categoryId l o =
  select $ do
    (product :& cat :& root) <-
      from $
        Table @Product
          `InnerJoin` Table @Category
          `on` (\(product :& sub) -> sub ^. CategoryId ==. product ^. ProductCategory)
          `LeftOuterJoin` Table @Category
          `on` (\(_ :& sub :& root) -> root ?. CategoryId ==. sub ^. CategoryParentId)
    orderBy [asc (product ^. ProductTitle)]
    where_ $
      cat ^. CategoryId ==. val categoryId
        ||. root ?. CategoryId ==. val (Just categoryId)
    limit l
    offset o
    pure product

getProductsR :: Handler Value
getProductsR = do
  (o, l) <- params
  products <- runDB $ select $ do
    ps <- from $ Table @Product
    orderBy [asc (ps ^. ProductTitle)]
    limit l
    offset o
    pure ps
  pure $ object ["status" .= ("ok" :: Text), "result" .= products]
