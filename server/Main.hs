{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (forever, forM_)
import           Data.Aeson (Value, FromJSON, parseJSON, Options(constructorTagModifier, sumEncoding, tagSingleConstructors, allNullaryToStringTag, fieldLabelModifier), (.:), ToJSON)
import qualified Data.Aeson as JSON
import           Data.Char (toLower)
import qualified Data.IORef as Ref
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           GHC.Generics
import           GHC.IO (unsafePerformIO)
import           Network.WebSockets (PendingConnection)
import qualified Network.WebSockets as WS
import qualified System.Random as R
import qualified Data.Maybe as Maybe

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
             | JoinSession { sessionId :: String }
             | LeaveSession
             deriving (Generic, Show)

instance FromJSON Request where
  parseJSON = JSON.genericParseJSON requestJSONOptions

data Response = Success { response_id :: Maybe Int, result :: String }
              | Notify { result :: String }
              | Failure { response_id :: Maybe Int, error :: String }
  deriving (Generic, Show)

responseJSONOptions = JSON.defaultOptions
  { fieldLabelModifier = T.unpack . stripPrefix "response_" . T.pack
  , sumEncoding = JSON.UntaggedValue
  }

instance ToJSON Response where
  toEncoding = JSON.genericToEncoding responseJSONOptions

type ConnectionId = Int
data Connection = Connection
  { con_id :: ConnectionId
  , con_ws :: WS.Connection
  }

instance Show Connection where
  show conn = show (con_id conn)

type SessionId = String
type State = [(Connection, Maybe SessionId)]
type StateRef = Ref.IORef State

hasConnection :: Connection -> State -> Bool
hasConnection conn = any ((== con_id conn) . con_id . fst)

hasSession :: SessionId -> State -> Bool
hasSession sessionId = any ((== Just sessionId) . snd)

newSession :: Connection -> StateRef -> IO (Either String String)
newSession conn state = do
  sessionId <- show <$> R.randomRIO (100000, 999999 :: Int)
  result <- Ref.atomicModifyIORef' state (setConnectionSessionId conn sessionId)
  case result of
    Left err -> do
      putStrLn $ "FAILURE: newSession: sessionId=" <> sessionId <> ", conn=" <> show conn <> ": " <> err
      pure $ Left err
    result -> do
      putStrLn $ "newSession: sessionId=" <> sessionId <> ", conn=" <> show conn
      pure $ Right sessionId

setConnectionSessionId :: Connection -> SessionId -> State -> (State, Either String String)
setConnectionSessionId conn sessionId state
  | not $ hasConnection conn state = (state, Left $ "No such connection: " <> show conn)
  | otherwise = (map setSessionId state, Right "OK")
  where
    setSessionId x@(c, _)
      | con_id c == con_id conn = (c, Just sessionId)
      | otherwise = x

joinSession :: Connection -> SessionId -> StateRef -> IO (Either String String)
joinSession conn sessionId state = do
  result <- Ref.atomicModifyIORef' state (addConnectionToSession sessionId conn)
  case result of
    Left err -> do
      putStrLn $ "FAILURE: joinSession: sessionId=" <> sessionId <> ", conn=" <> show conn <> ": " <> err
      pure $ Left err
    Right others -> do
      putStrLn $ "joinSession: sessionId=" <> sessionId <> ", conn=" <> show conn
      mapM_ notifyJoin others
      pure $ Right "OK"
  where
    addConnectionToSession sessionId conn state
      | not $ hasSession sessionId state = (state, Left $ "No such session: " <> sessionId)
      | otherwise = ((conn, Just sessionId) : state, Right $ findOthers state)
    findOthers = map fst . filter ((== Just sessionId) . snd)
    notifyJoin other = do
      putStrLn $ "joinSession: notifyJoin conn=" <> show conn
      WS.sendTextData (con_ws other) (JSON.encode $ Notify "peerJoined")

leaveSession :: Connection -> StateRef -> IO (Either String String)
leaveSession conn state = do
  result <- Ref.atomicModifyIORef' state (removeConnectionFromSession conn)
  case result of
    Left err -> do
      putStrLn $ "FAILURE: leaveSession: conn=" <> show conn <> ": " <> err
      pure $ Left err
    result -> do
      putStrLn $ "leaveSession: conn=" <> show conn
      pure result
  where
    removeConnectionFromSession conn state
      | not $ hasConnection conn state = (state, Left $ "No such connection: " <> show conn)
      | otherwise = (map removeConn state, Right "OK")
    removeConn x@(c, _)
      | con_id c == con_id conn = (c, Nothing)
      | otherwise = x

app :: IO Int -> StateRef -> PendingConnection -> IO ()
app getConnId state req = do
  conn <- Connection <$> getConnId <*> WS.acceptRequest req
  Ref.atomicModifyIORef' state $ \cs -> ((conn, Nothing) : cs, ())
  forever $ do
    msg <- JSON.eitherDecode <$> WS.receiveData (con_ws conn)
    response <- case msg of
      Left err -> pure $ Failure Nothing err
      Right (RequestWithId id request) -> do
        result <- case request of
          NewSession -> newSession conn state
          JoinSession sessionId -> joinSession conn sessionId state
          LeaveSession -> leaveSession conn state
        pure $ case result of
          Left err -> Failure (Just id) err
          Right res -> Success (Just id) res
    WS.sendTextData (con_ws conn) (JSON.encode response)

main :: IO ()
main = do
  let host = "localhost"
      port = 3001
  putStrLn $ "Starting server on " <> host <> ":" <> show port
  nextId <- Ref.newIORef 1
  let getNextId = Ref.atomicModifyIORef' nextId $ \id -> (id + 1, id)
  state <- Ref.newIORef []
  WS.runServer host port (app getNextId state)
