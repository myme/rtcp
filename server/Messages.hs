{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Messages
  ( Request (..),
    RequestWithId (..),
    Response (..),
    stripPrefix,
  )
where

import Data.Aeson (FromJSON, Options (allNullaryToStringTag, constructorTagModifier, fieldLabelModifier, sumEncoding), ToJSON, (.:))
import qualified Data.Aeson as JSON
import Data.Char (toLower)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

stripPrefix :: String -> String -> String
stripPrefix prefix text = fromMaybe text $ List.stripPrefix prefix text

requestJSONOptions :: Options
requestJSONOptions =
  JSON.defaultOptions
    { allNullaryToStringTag = False,
      constructorTagModifier = \(c : cs) -> toLower c : cs,
      fieldLabelModifier = stripPrefix "request_",
      sumEncoding = JSON.TaggedObject "method" "method"
    }

data RequestWithId = RequestWithId
  { request_id :: Int,
    request :: Request
  }
  deriving (Show)

instance FromJSON RequestWithId where
  parseJSON = JSON.withObject "RequestWithId" $ \obj ->
    RequestWithId <$> (obj .: "id") <*> JSON.parseJSON (JSON.toJSON obj)

data Request
  = GetIceConfig {request_hostname :: String}
  | NewSession
  | JoinSession {request_sessionId :: String, request_sessionPin :: String}
  | LeaveSession
  | Broadcast {request_params :: JSON.Value}
  deriving (Generic, Show)

instance FromJSON Request where
  parseJSON = JSON.genericParseJSON requestJSONOptions

data Response
  = Success {response_id :: Maybe Int, response_result :: JSON.Value}
  | Notify {response_method :: String, response_params :: Maybe JSON.Value}
  | Failure {response_id :: Maybe Int, response_error :: String}
  deriving (Generic, Show)

responseJSONOptions :: Options
responseJSONOptions =
  JSON.defaultOptions
    { fieldLabelModifier = stripPrefix "response_",
      sumEncoding = JSON.UntaggedValue
    }

instance ToJSON Response where
  toJSON = JSON.genericToJSON responseJSONOptions
  toEncoding = JSON.genericToEncoding responseJSONOptions
