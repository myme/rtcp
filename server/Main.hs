{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (forever, forM_)
import           Data.Aeson (FromJSON, Options(constructorTagModifier, sumEncoding, allNullaryToStringTag, fieldLabelModifier), (.:), ToJSON)
import qualified Data.Aeson as JSON
import           Data.Char (toLower)
import qualified Data.IORef as Ref
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           GHC.Generics
import qualified Network.WebSockets as WS
import qualified System.Random as R

stripPrefix :: T.Text -> T.Text -> T.Text
stripPrefix prefix text = fromMaybe text $ T.stripPrefix prefix text

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
  result <- Ref.atomicModifyIORef' state addConnectionToSession
  case result of
    Left err -> do
      putStrLn $ "FAILURE: joinSession: sessionId=" <> sessionId <> ", conn=" <> show conn <> ": " <> err
      pure $ Left err
    Right others -> do
      putStrLn $ "joinSession: sessionId=" <> sessionId <> ", conn=" <> show conn
      mapM_ notifyJoin others
      pure $ Right "OK"
  where
    addConnectionToSession state
      | not $ hasSession sessionId state = (state, Left $ "No such session: " <> sessionId)
      | otherwise = ((conn, Just sessionId) : state, Right $ findOthers state)
    findOthers = map fst . filter ((== Just sessionId) . snd)
    notifyJoin other = do
      putStrLn $ "joinSession: notifyJoin conn=" <> show conn
      WS.sendTextData (con_ws other) (JSON.encode $ Notify "peerJoined" Nothing)

leaveSession :: Connection -> StateRef -> IO (Either String String)
leaveSession conn state = do
  result <- Ref.atomicModifyIORef' state removeConnectionFromSession
  case result of
    Left err -> do
      putStrLn $ "FAILURE: leaveSession: conn=" <> show conn <> ": " <> err
      pure $ Left err
    result -> do
      putStrLn $ "leaveSession: conn=" <> show conn
      pure result
  where
    removeConnectionFromSession state
      | not $ hasConnection conn state = (state, Left $ "No such connection: " <> show conn)
      | otherwise = (map removeConn state, Right "OK")
    removeConn x@(c, _)
      | con_id c == con_id conn = (c, Nothing)
      | otherwise = x

broadcast :: JSON.Value -> Connection -> StateRef -> IO (Either String String)
broadcast payload conn state = do
  result <- findOthers <$> Ref.readIORef state
  case result of
    Left err -> do
      putStrLn $ "FAILURE: broadcst: " <> err
      pure $ Left err
    Right (sessionId, others) -> do
      forM_ others $ \(other, _) -> do
        putStrLn
          $  "broadcast: sessionId=" <> sessionId
          <> ", from=" <> show conn
          <> ", to=" <> show other
        WS.sendTextData (con_ws other) (JSON.encode $ Notify "broadcast" (Just payload))
      pure $ Right "OK"
  where
    findOthers state = case List.find ((== con_id conn) . con_id . fst) state of
      Nothing -> Left $ "No such connection: conn=" <> show conn
      Just (_, Nothing) -> Left $ "Connection is not in a session: conn=" <> show conn
      Just (_, Just sessionId) ->
        let sameSession = filter ((== Just sessionId) . snd) state
            others = filter ((/= con_id conn) . con_id . fst) sameSession
        in Right (sessionId, others)

app :: IO Int -> StateRef -> WS.PendingConnection -> IO ()
app getConnId state req = do
  conn <- Connection <$> getConnId <*> WS.acceptRequest req
  Ref.atomicModifyIORef' state $ \cs -> ((conn, Nothing) : cs, ())
  forever $ do
    msg <- JSON.eitherDecode <$> WS.receiveData (con_ws conn)
    response <- case msg of
      Left err -> do
        putStrLn $ "FAILURE: app: " <> err
        pure $ Failure Nothing err
      Right (RequestWithId id request) -> do
        result <- case request of
          NewSession -> newSession conn state
          JoinSession sessionId -> joinSession conn sessionId state
          LeaveSession -> leaveSession conn state
          Broadcast payload -> broadcast payload conn state
        pure $ case result of
          Left err -> Failure (Just id) err
          Right res -> Success (Just id) (JSON.String $ T.pack res)
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
