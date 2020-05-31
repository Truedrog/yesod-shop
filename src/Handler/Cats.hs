{-# LANGUAGE TypeApplications #-}

module Handler.Cats where

import qualified Data.Map.Strict as M
import Database.Esqueleto hiding (Value, from, on)
import Database.Esqueleto.Experimental
import Import hiding ((==.), isNothing, on)

data Link
  = Link
      { catId :: Maybe Int64,
        title :: Text,
        url :: Maybe Text,
        links :: Maybe [Link]
      }
  deriving (Show, Eq, Ord, Generic)

instance Monoid Link where
  mempty = Link {catId = Nothing, title = "", url = Just "", links = Nothing}

instance Semigroup Link where
  Link _ _ _ links <> Link catId title url links' = Link catId title url (links <> links')

instance FromJSON Link

instance ToJSON Link where
  toJSON (Link catId title url links) =
    object $
      stripNulls
        [ "id" .= catId,
          "title" .= title,
          "url" .= url,
          "links" .= links
        ]

getCatsR :: Handler Value
getCatsR = do
  cats <- runDB selectCats
  let ks =
        M.elems $
          M.fromListWith
            (<>)
            [ ( cId,
                Link {catId = Just cId, title = title, url = url, links = Just [Link {catId = subCatId, title = subTitle, url = subUrl, links = Nothing}]}
              )
              | (cat, subcat) <- cats,
                let cId = fromSqlKey . entityKey $ cat
                    title = categoryTitle . entityVal $ cat
                    url = categoryUrl . entityVal $ cat
                    subCatId = fromSqlKey . entityKey <$> subcat
                    subTitle = maybe "" (categoryTitle . entityVal) subcat
                    subUrl = (categoryUrl . entityVal) =<< subcat
            ]
  pure $ object ["status" .= ("ok" :: Text), "result" .= ks]

selectCats :: DB [(Entity Category, Maybe (Entity Category))]
selectCats =
  select $ do
    (root :& subcat) <-
      from $
        Table @Category
          `LeftOuterJoin` Table @Category
          `on` (\(root :& subcat) -> just (just (root ^. CategoryId)) ==. subcat ?. CategoryParentId)
    where_ (isNothing (just (root ^. CategoryParentId)))
    pure (root, subcat)
