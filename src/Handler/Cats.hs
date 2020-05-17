{-# LANGUAGE TypeApplications #-}

module Handler.Cats where

import qualified Data.Map.Strict as M
import Database.Esqueleto hiding (Value, from, on)
import Database.Esqueleto.Experimental
import Import hiding ((==.), isNothing, on)

data Link
  = Link
      { title :: Maybe Text,
        url :: Maybe Text,
        links :: Maybe [Link]
      }
  deriving (Show, Eq, Ord, Generic)

instance Monoid Link where
  mempty = Link {title = Just "", url = Nothing, links = Nothing}

instance Semigroup Link where
  Link _ _ links <> Link title url links' = Link title url (links <> links')

instance FromJSON Link

instance ToJSON Link where
  toJSON (Link title url links) =
    object $
      stripNulls
        [ "title" .= title,
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
            [ ( catId,
                Link {title = Just title, url = url, links = Just [Link {title = subTitle, url = subUrl, links = Nothing}]}
              )
              | (cat, subcat) <- cats,
                let catId = fromSqlKey . entityKey $ cat
                    title = categoryTitle . entityVal $ cat
                    url = categoryUrl . entityVal $ cat
                    subTitle = categoryTitle . entityVal <$> subcat
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
