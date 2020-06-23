module Handler.Cats where

import Data.Aeson
import qualified Data.Map.Strict as M
import Database.Esqueleto hiding (Value, from, on)
import Database.Esqueleto.Experimental
import Import hiding ((==.), isNothing, on)

data Link
  = Link
      { cId :: !Int64,
        title :: !Text,
        url :: !(Maybe Text),
        links :: !(Maybe [Link])
      }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Link where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance ToJSON Link where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}


getCatsR :: Handler Value
getCatsR = do
  cats <- runDB selectCats
  let ks =
        fmap (\((cId, url, title), links) -> Link {cId = cId, title = title, url = url, links = Just links}) . M.toList $
          M.fromListWith
            (<>)
            [ ((cId, url, title), [Link {cId = subCatId, title = subTitle, url = subUrl, links = Nothing}])
              | (category, subcat) <- cats,
                let cId = fromSqlKey . entityKey $ category
                    title = categoryTitle . entityVal $ category
                    url = categoryUrl . entityVal $ category
                    subCatId = maybe 0 (fromSqlKey . entityKey) subcat
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