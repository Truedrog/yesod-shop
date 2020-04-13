{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Data where

import Data.Aeson
import Database.Esqueleto
import Import

newtype MyResponse
  = MyResponse
      { msg :: Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON MyResponse where
  toEncoding = genericToEncoding defaultOptions

newtype Person
  = Person
      { age :: Int
      }
  deriving (Show, Eq, Generic)

instance FromJSON Person

instance ToJSON Person where
  toEncoding = genericToEncoding defaultOptions

getDataR :: Handler Import.Value
getDataR = do
  users <- runDB getUsers
  returnJson users
  where
    getUsers :: (MonadIO m) => SqlReadT m [Entity User]
    getUsers = select $ from $ \user -> pure user
