{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Applicative ((<**>))
import Control.Exception (catch)
import Control.Monad (forM_, void, (>=>))
import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.IORef as Ref
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Log
import qualified Messages as Msg
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.Wai.Middleware.Rewrite as Rewrite
import qualified Network.Wai.Middleware.Static as Static
import qualified Network.WebSockets as WS
import qualified Options.Applicative as Opts
import qualified System.Random as R
import Prelude hiding (id)

type ConnectionId = Int

data Connection = Connection
  { con_id :: ConnectionId,
    con_ws :: WS.Connection
  }

instance Show Connection where
  show conn = show (con_id conn)

data IceConfig = IceConfig
  { ice_urls :: [Text],
    ice_user :: Maybe Text,
    ice_pass :: Maybe Text
  }
  deriving (Generic)

iceJSONOptions :: JSON.Options
iceJSONOptions =
  JSON.defaultOptions
    { JSON.fieldLabelModifier = Msg.stripPrefix "ice_",
      JSON.omitNothingFields = True,
      JSON.sumEncoding = JSON.UntaggedValue
    }

instance ToJSON IceConfig where
  toJSON = JSON.genericToJSON iceJSONOptions
  toEncoding = JSON.genericToEncoding iceJSONOptions

type SessionId = String

type SessionPin = String

data Session = Session
  { session_id :: SessionId,
    session_pin :: SessionPin
  }
  deriving (Generic, Show)

sessionJSONOptions :: JSON.Options
sessionJSONOptions =
  JSON.defaultOptions
    { JSON.fieldLabelModifier = Msg.stripPrefix "session_",
      JSON.omitNothingFields = True,
      JSON.sumEncoding = JSON.UntaggedValue
    }

instance ToJSON Session where
  toJSON = JSON.genericToJSON sessionJSONOptions
  toEncoding = JSON.genericToEncoding sessionJSONOptions

type Sessions = Map SessionId Session

type Connections = [(Connection, Maybe SessionId)]

data State = State
  { connections :: Connections,
    sessions :: Sessions
  }
  deriving (Show)

type StateRef = Ref.IORef State

hasConnection :: Connection -> State -> Bool
hasConnection conn = any ((== con_id conn) . con_id . fst) . connections

sameConnection :: Connection -> (Connection, a) -> Bool
sameConnection conn = (== con_id conn) . con_id . fst

sameSession :: SessionId -> (a, Maybe SessionId) -> Bool
sameSession sessionId = (== Just sessionId) . snd

leftPad :: Int -> Char -> String -> String
leftPad n c s = replicate (n - length s) c <> s

getIceConfig :: String -> [IceConfig]
getIceConfig hostname =
  [IceConfig ["stun:" <> T.pack hostname <> ":3478"] Nothing Nothing]

newSession :: Connection -> StateRef -> IO (Either String Session)
newSession conn stateRef = do
  sessionId <- leftPad 6 '0' . show <$> R.randomRIO (0, 999999 :: Int)
  sessionPin <- leftPad 4 '0' . show <$> R.randomRIO (0, 9999 :: Int)
  let session = Session sessionId sessionPin
  result <- Ref.atomicModifyIORef' stateRef (tryUpdate $ addSession session >=> addConnectionToSession conn session)
  case result of
    Left err -> do
      Log.err $ "FAILURE: newSession: sessionId=" <> sessionId <> ", conn=" <> show conn <> ": " <> err
      pure $ Left err
    _ -> do
      Log.info $ "newSession: sessionId=" <> sessionId <> ", conn=" <> show conn
      pure . Right $ session

tryUpdate :: (State -> Either String State) -> State -> (State, Either String State)
tryUpdate f state = case f state of
  Left err -> (state, Left err)
  Right newState -> (newState, Right newState)

addSession :: Session -> State -> Either String State
addSession session (State cs ss)
  | Map.member (session_id session) ss = Left $ "Session already exists: " <> show session
  | otherwise = Right $ State cs (Map.insert (session_id session) session ss)

addConnectionToSession :: Connection -> Session -> State -> Either String State
addConnectionToSession conn (Session id' pin) state@(State cs ss)
  | not $ hasConnection conn state = Left $ "No such connection: " <> show conn
  | not $ Map.member id' ss = Left $ "No such session: " <> id'
  | not hasValidPin = Left $ "Invalid PIN for session: " <> id'
  | otherwise = Right $ State (map setConnSessionId cs) ss
  where
    hasValidPin = (== Just pin) $ session_pin <$> Map.lookup id' ss
    setConnSessionId c@(other, _)
      | con_id other == con_id conn = (other, Just id')
      | otherwise = c

joinSession :: Connection -> Session -> StateRef -> IO (Either String ())
joinSession conn session state = do
  result <- Ref.atomicModifyIORef' state (tryUpdate $ addConnectionToSession conn session)
  case result of
    Left err -> do
      Log.err $
        "FAILURE: joinSession: sessionId=" <> session_id session <> ", conn=" <> show conn <> ": " <> err
      pure $ Left err
    Right state' -> do
      Log.info $
        "joinSession: sessionId=" <> session_id session <> ", conn=" <> show conn
      let isOther c = not (sameConnection conn c) && sameSession (session_id session) c
          others = map fst . filter isOther $ connections state'
      forM_ others $ \other -> do
        Log.info $ "joinSession: notifyJoin conn=" <> show conn
        WS.sendTextData (con_ws other) (JSON.encode $ Msg.Notify "peerJoined" Nothing)
      pure $ Right ()

leaveSession :: Connection -> StateRef -> IO (Either String ())
leaveSession conn stateRef = do
  result <- Ref.atomicModifyIORef' stateRef (tryUpdate $ removeConnection conn)
  case result of
    Left err -> do
      Log.err $ "FAILURE: leaveSession: conn=" <> show conn <> ": " <> err
      pure $ Left err
    _ -> do
      Log.info $ "leaveSession: conn=" <> show conn
      pure $ Right ()

removeConnection :: Connection -> State -> Either String State
removeConnection conn state@(State cs ss)
  | not $ hasConnection conn state = Left $ "No such connection: " <> show conn
  | otherwise = case List.find (sameConnection conn) cs of
      Just (_, Just sessionId) ->
        let newConnections = map unsetSessionId cs
            newSessions =
              if any ((== Just sessionId) . snd) cs
                then ss
                else Map.delete sessionId ss
         in Right $ State newConnections newSessions
      _ -> Left $ "Connection is not attached to a session: " <> show conn
  where
    unsetSessionId x@(c, _)
      | con_id c == con_id conn = (c, Nothing)
      | otherwise = x

broadcast :: JSON.Value -> Connection -> StateRef -> IO (Either String String)
broadcast payload conn state = do
  result <- findOthers . connections <$> Ref.readIORef state
  case result of
    Left err -> do
      Log.err $ "FAILURE: broadcast: " <> err
      pure $ Left err
    Right (sessionId, others) -> do
      forM_ others $ \(other, _) -> do
        Log.info $
          "broadcast: sessionId="
            <> sessionId
            <> ", from="
            <> show conn
            <> ", to="
            <> show other
        WS.sendTextData (con_ws other) (JSON.encode $ Msg.Notify "broadcast" (Just payload))
      pure $ Right "OK"
  where
    findOthers state' = case List.find (sameConnection conn) state' of
      Nothing -> Left $ "No such connection: conn=" <> show conn
      Just (_, Nothing) -> Left $ "Connection is not in a session: conn=" <> show conn
      Just (_, Just sessionId) ->
        let others = filter (not . sameConnection conn) $ filter (sameSession sessionId) state'
         in Right (sessionId, others)

wsApp :: IO Int -> StateRef -> WS.ServerApp
wsApp getConnId stateRef req = do
  conn <- Connection <$> getConnId <*> WS.acceptRequest req
  Ref.atomicModifyIORef' stateRef $ \state -> (addConnection conn state, ())
  go conn
  where
    addConnection conn state@(State cs _) = state {connections = (conn, Nothing) : cs}
    toJSON :: (Functor f, ToJSON a) => f a -> f JSON.Value
    toJSON = fmap JSON.toJSON
    go conn = do
      wsRead <- (Right . JSON.eitherDecode <$> WS.receiveData (con_ws conn)) `catch` handleWSError stateRef conn
      case wsRead of
        Left err -> Log.err $ "FAILURE: WebSocket: " <> err
        Right msg -> do
          response <- case msg of
            Left err -> do
              Log.err $ "FAILURE: handle message: " <> err
              pure $ Msg.Failure Nothing err
            Right (Msg.RequestWithId id request) -> do
              result <- case request of
                Msg.GetIceConfig hostname -> pure . Right . JSON.toJSON $ getIceConfig hostname
                Msg.NewSession -> toJSON <$> newSession conn stateRef
                Msg.JoinSession id' pin -> toJSON <$> joinSession conn (Session id' pin) stateRef
                Msg.LeaveSession -> toJSON <$> leaveSession conn stateRef
                Msg.Broadcast payload -> toJSON <$> broadcast payload conn stateRef
              pure $ case result of
                Left err -> Msg.Failure (Just id) err
                Right res -> Msg.Success (Just id) res
          WS.sendTextData (con_ws conn) (JSON.encode response)
          go conn

handleWSError :: StateRef -> Connection -> WS.ConnectionException -> IO (Either String a)
handleWSError state conn = \case
  WS.CloseRequest _ _ -> do
    cleanupConnection
    pure $ Left "Connection closing"
  WS.ConnectionClosed -> do
    cleanupConnection
    pure $ Left "Connection closed"
  _ -> undefined
  where
    cleanupConnection = void $ Ref.atomicModifyIORef' state (tryUpdate $ removeConnection conn)

wsMiddleware :: IO Int -> StateRef -> Wai.Middleware
wsMiddleware getNextId stateRef =
  WS.websocketsOr WS.defaultConnectionOptions (wsApp getNextId stateRef)

routeMiddleware :: Wai.Middleware
routeMiddleware = Rewrite.rewritePureWithQueries rewrite
  where
    rewrite paths _ = case paths of
      ("assets" : _, _) -> paths
      _ -> (["index.html"], [])

staticMiddleware :: FilePath -> Wai.Middleware
staticMiddleware staticRoot = Static.staticPolicy policy
  where
    policy = Static.addBase staticRoot

fallbackApp :: Wai.Application
fallbackApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a WebSocket request"

data Options = Options
  { opts_host :: String,
    opts_port :: Int,
    opts_static_root :: FilePath
  }

optionsParser :: Opts.Parser Options
optionsParser =
  Options
    <$> Opts.strOption
      (Opts.long "host" <> Opts.value "localhost" <> Opts.help "bind to host")
    <*> Opts.option
      Opts.auto
      ( Opts.long "port" <> Opts.value 8000 <> Opts.help "listen to port"
      )
    <*> Opts.strOption
      ( Opts.long "static-root" <> Opts.value "./" <> Opts.help "serve static files from dir"
      )

main :: IO ()
main = do
  opts <-
    Opts.execParser $
      Opts.info
        (optionsParser <**> Opts.helper)
        ( Opts.fullDesc
            <> Opts.progDesc "rtcp server"
            <> Opts.header "rtcp server - exchange stuff between things using WebRTC"
        )

  let host = opts_host opts
      port = opts_port opts
      staticRoot = opts_static_root opts

  nextId <- Ref.newIORef 1
  let getNextId = Ref.atomicModifyIORef' nextId $ \id -> (id + 1, id)
  state <- Ref.newIORef (State [] Map.empty)

  Log.info $ "Starting server on http://" <> host <> ":" <> show port
  Warp.run port $
    wsMiddleware getNextId state $
      routeMiddleware $
        staticMiddleware
          staticRoot
          fallbackApp
