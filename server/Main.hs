{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (forever)
import           Data.Aeson (Value, FromJSON, parseJSON, Options(constructorTagModifier, sumEncoding, tagSingleConstructors, allNullaryToStringTag, fieldLabelModifier), (.:), ToJSON)
import qualified Data.Aeson as JSON
import           Data.Char (toLower)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           GHC.Generics
import           Network.WebSockets (PendingConnection, Connection)
import qualified Network.WebSockets as WS

stripPrefix :: T.Text -> T.Text -> T.Text
stripPrefix prefix text = fromMaybe text $ T.stripPrefix prefix text

requestJSONOptions = JSON.defaultOptions
  { allNullaryToStringTag = False
  , constructorTagModifier = \(c:cs) -> toLower c : cs
  , sumEncoding = JSON.TaggedObject "method" "method"
  }

data RequestWithId = RequestWithId
  { request_id :: Int
  , request :: Request
  } deriving Show

instance FromJSON RequestWithId where
  parseJSON = JSON.withObject "RequestWithId" $ \obj ->
    RequestWithId <$> (obj .: "id") <*> parseJSON (JSON.toJSON obj)

data Request = NewSession
             | JoinSession
             deriving (Generic, Show)

instance FromJSON Request where
  parseJSON = JSON.genericParseJSON requestJSONOptions

data Response = Success { response_id :: Maybe Int, result :: String }
              | Failure { response_id :: Maybe Int, error :: String }
  deriving (Generic, Show)

responseJSONOptions = JSON.defaultOptions
  { fieldLabelModifier = T.unpack . stripPrefix "response_" . T.pack
  , sumEncoding = JSON.UntaggedValue
  }

instance ToJSON Response where
  toEncoding = JSON.genericToEncoding responseJSONOptions

handleRequest :: Request -> IO String
handleRequest NewSession = pure "sessionId"
handleRequest JoinSession = pure "session joined"

app :: PendingConnection -> IO ()
app req = do
  conn <- WS.acceptRequest req
  forever $ do
    msg <- JSON.eitherDecode <$> WS.receiveData conn
    T.putStrLn $ "Got message: " <> T.pack (show (msg :: Either String RequestWithId))
    response <- case msg of
          Left err -> pure $ Failure Nothing err
          Right (RequestWithId id request) -> do
            result <- handleRequest request
            pure $ Success (Just id) result
    WS.sendTextData conn (JSON.encode response)

main :: IO ()
main = do
  let host = "localhost"
      port = 3001
  putStrLn $ "Starting server on " <> host <> ":" <> show port
  WS.runServer host port app
