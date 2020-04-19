module Db.DbTypes where

import ClassyPrelude.Yesod

data ProductSpecType
  = ProductSpecType {specName :: Text, features :: [ProductFeature]}
  deriving (Show, Eq, Generic)

instance FromJSON ProductSpecType
instance ToJSON ProductSpecType

data ProductFeature
 = ProductFeature {featureName :: Text, value :: Text}
 deriving (Show, Eq, Generic)

instance FromJSON ProductFeature
instance ToJSON ProductFeature
