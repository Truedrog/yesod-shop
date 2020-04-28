{-# LANGUAGE TypeApplications #-}

module Handler.Cats where

import qualified Data.Map as M
import Database.Esqueleto hiding (Value, from, on)
import Database.Esqueleto.Experimental
import Import hiding ((==.), isNothing, on)

data Link
  = Link
      { title :: Maybe Text,
        links :: Maybe [Link]
      }
  deriving (Show, Eq, Ord, Generic)

instance Monoid Link where
  mempty = Link {title = Just "", links = Nothing}

instance Semigroup Link where
  Link _ links <> Link title links' = Link title (links <> links')

instance FromJSON Link

instance ToJSON Link where
  toJSON (Link title links) =
    object $
      stripNulls
        [ "title" .= title,
          "links" .= links
        ]

getCatsR :: Handler Value
getCatsR = do
  cats <- runDB selectCats
  returnJson $ M.elems $
    M.fromListWith
      (<>)
      [ ( fromSqlKey catId,
          Link {title = Just title, links = Just [Link {title = subTitle, links = Nothing}]}
        )
        | (cat, subcat) <- cats,
          let catId = entityKey cat,
          let title = categoryTitle . entityVal $ cat,
          let subTitle = categoryTitle . entityVal <$> subcat
      ]

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
