{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Data where

import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy.Encoding as TL
import Import

newtype MyResponse =
  MyResponse
    { msg :: Text
    }
  deriving (Show, Eq, Generic)

instance ToJSON MyResponse where
  toEncoding = genericToEncoding defaultOptions

newtype Person =
  Person
    { age :: Int
    }
  deriving (Show, Eq, Generic)

instance FromJSON Person

instance ToJSON Person where
  toEncoding = genericToEncoding defaultOptions

getDataR :: Handler Value
getDataR = do
  mayeObjectInSession <- lookupSession "object11"
  case mayeObjectInSession of
    Nothing -> do
      let person = Person 29
      let person2 = Person 13
  --     setSession "object" ((toStrict $ encodeToLazyText person) :: Text)
  --     setSession "object1" ((toStrict $ encodeToLazyText person2) :: Text)
  --     setSession "object2" ((toStrict $ encodeToLazyText person2) :: Text)
  --     setSession "object3" ((toStrict $ encodeToLazyText person) :: Text)
  --     setSession "object4" ((toStrict $ encodeToLazyText person2) :: Text)
  --     setSession "object5" ((toStrict $ encodeToLazyText person) :: Text)
  --     setSession "object6" ((toStrict $ encodeToLazyText person2) :: Text)
  --     setSession "object7" ((toStrict $ encodeToLazyText person) :: Text)
  --     setSession "object8" ((toStrict $ encodeToLazyText person2) :: Text)
  --     setSession "object9" ((toStrict $ encodeToLazyText person2) :: Text)
  --     setSession "object10" ((toStrict $ encodeToLazyText person) :: Text)
      setSession "object11" ((toStrict $ encodeToLazyText person2) :: Text)
      returnJson person
    Just objectInSession ->
      case (decode $ TL.encodeUtf8 $ fromStrict objectInSession :: Maybe Person) of
        Just o -> do
          liftIO $ print o
          sendStatusJSON status201 o
        Nothing -> sendStatusJSON status400 (MyResponse {msg = "Some error happend in decode"})