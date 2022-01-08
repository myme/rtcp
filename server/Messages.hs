{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Messages
 ( Request(..)
 , RequestWithId(..)
 , Response(..)
 ) where

import           Data.Aeson (FromJSON, Options(constructorTagModifier, sumEncoding, allNullaryToStringTag, fieldLabelModifier), (.:), ToJSON)
import qualified Data.Aeson as JSON
import           Data.Char (toLower)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           GHC.Generics

stripPrefix :: T.Text -> T.Text -> T.Text
stripPrefix prefix text = fromMaybe text $ T.stripPrefix prefix text

requestJSONOptions :: Options
requestJSONOptions = JSON.defaultOptions
  { allNullaryToStringTag = False
  , constructorTagModifier = \(c:cs) -> toLower c : cs
  , fieldLabelModifier = T.unpack . stripPrefix "request_" . T.pack
  , sumEncoding = JSON.TaggedObject "method" "method"
  }

data RequestWithId = RequestWithId
  { request_id :: Int
  , request :: Request
  } deriving Show

instance FromJSON RequestWithId where
  parseJSON = JSON.withObject "RequestWithId" $ \obj ->
    RequestWithId <$> (obj .: "id") <*> JSON.parseJSON (JSON.toJSON obj)

data Request = NewSession
             | JoinSession { request_sessionId :: String }
             | LeaveSession
             | Broadcast { request_params :: JSON.Value }
             deriving (Generic, Show)

instance FromJSON Request where
  parseJSON = JSON.genericParseJSON requestJSONOptions

data Response = Success { response_id :: Maybe Int, response_result :: JSON.Value }
              | Notify { response_method :: String, response_params :: Maybe JSON.Value }
              | Failure { response_id :: Maybe Int, response_error :: String }
  deriving (Generic, Show)

responseJSONOptions :: Options
responseJSONOptions = JSON.defaultOptions
  { fieldLabelModifier = T.unpack . stripPrefix "response_" . T.pack
  , sumEncoding = JSON.UntaggedValue
  }

instance ToJSON Response where
  toEncoding = JSON.genericToEncoding responseJSONOptions
