{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<**>))
import Control.Exception (catch)
import Control.Monad (forM_)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.IORef as Ref
import qualified Data.List as List
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

type State = [(Connection, Maybe SessionId)]

type StateRef = Ref.IORef State

hasConnection :: Connection -> State -> Bool
hasConnection conn = any ((== con_id conn) . con_id . fst)

hasSession :: SessionId -> State -> Bool
hasSession sessionId = any ((== Just sessionId) . snd)

sameConnection :: Connection -> (Connection, a) -> Bool
sameConnection conn = (== con_id conn) . con_id . fst

sameSession :: SessionId -> (a, Maybe SessionId) -> Bool
sameSession sessionId = (== Just sessionId) . snd

leftPad :: Int -> Char -> String -> String
leftPad n c s = replicate (n - length s) c <> s

getIceConfig :: String -> [IceConfig]
getIceConfig hostname =
  [IceConfig ["stun:" <> T.pack hostname <> ":3478"] Nothing Nothing]

newSession :: Connection -> StateRef -> IO (Either String String)
newSession conn state = do
  sessionId <- leftPad 6 '0' . show <$> R.randomRIO (0, 999999 :: Int)
  result <- Ref.atomicModifyIORef' state (setConnectionSessionId conn sessionId)
  case result of
    Left err -> do
      Log.err $ "FAILURE: newSession: sessionId=" <> sessionId <> ", conn=" <> show conn <> ": " <> err
      pure $ Left err
    _ -> do
      Log.info $ "newSession: sessionId=" <> sessionId <> ", conn=" <> show conn
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
      Log.err $ "FAILURE: joinSession: sessionId=" <> sessionId <> ", conn=" <> show conn <> ": " <> err
      pure $ Left err
    Right others -> do
      Log.info $ "joinSession: sessionId=" <> sessionId <> ", conn=" <> show conn
      forM_ others $ \other -> do
        Log.info $ "joinSession: notifyJoin conn=" <> show conn
        WS.sendTextData (con_ws other) (JSON.encode $ Msg.Notify "peerJoined" Nothing)
      pure $ Right "OK"
  where
    addConnectionToSession state'
      | not $ hasSession sessionId state' = (state', Left $ "No such session: " <> sessionId)
      | otherwise =
        let (newState, result) = setConnectionSessionId conn sessionId state'
        in (newState, result >> Right (findOthers state'))
    findOthers = map fst . filter (sameSession sessionId)

leaveSession :: Connection -> StateRef -> IO (Either String String)
leaveSession conn state = do
  result <- Ref.atomicModifyIORef' state removeConnectionFromSession
  case result of
    Left err -> do
      Log.err $ "FAILURE: leaveSession: conn=" <> show conn <> ": " <> err
      pure $ Left err
    result' -> do
      Log.info $ "leaveSession: conn=" <> show conn
      pure result'
  where
    removeConnectionFromSession state'
      | not $ hasConnection conn state' = (state', Left $ "No such connection: " <> show conn)
      | otherwise = (map removeConn state', Right "OK")
    removeConn x@(c, _)
      | con_id c == con_id conn = (c, Nothing)
      | otherwise = x

broadcast :: JSON.Value -> Connection -> StateRef -> IO (Either String String)
broadcast payload conn state = do
  result <- findOthers <$> Ref.readIORef state
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
wsApp getConnId state req = do
  conn <- Connection <$> getConnId <*> WS.acceptRequest req
  Ref.atomicModifyIORef' state $ \cs -> ((conn, Nothing) : cs, ())
  go conn
  where
    jsonString = fmap (JSON.String . T.pack)
    go conn = do
      wsRead <- (Right . JSON.eitherDecode <$> WS.receiveData (con_ws conn)) `catch` handleWSError state conn
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
                Msg.NewSession -> jsonString <$> newSession conn state
                Msg.JoinSession sessionId -> jsonString <$> joinSession conn sessionId state
                Msg.LeaveSession -> jsonString <$> leaveSession conn state
                Msg.Broadcast payload -> jsonString <$> broadcast payload conn state
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
    cleanupConnection = Ref.atomicModifyIORef' state $ \cs -> (filter ((/= con_id conn) . con_id . fst) cs, ())

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
  state <- Ref.newIORef []

  Log.info $ "Starting server on http://" <> host <> ":" <> show port
  Warp.run port $
    wsMiddleware getNextId state $
      routeMiddleware $
        staticMiddleware
          staticRoot
          fallbackApp
