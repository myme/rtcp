{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Auth
  ( authMiddleware,
    mkOIDCConfig,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as JSON
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Function ((&))
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Vault.Lazy as Vault
import GHC.Generics (Generic)
import qualified Log
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hCookie, hLocation)
import qualified Network.HTTP.Types as Http
import Network.HTTP.Types.Header (hSetCookie)
import Network.Wai (Request (vault))
import qualified Network.Wai as Wai
import System.Environment (getEnv)
import qualified Web.ClientSession as Session
import qualified Web.Cookie as Cookie
import qualified Web.OIDC.Client as OIDC

data OIDCConfig = OIDCConfig
  { oidcDiscoveryUrl :: Text,
    oidcClientId :: BS.ByteString,
    oidcClientSecret :: BS.ByteString,
    oidcFrontendUri :: BS.ByteString,
    oidcRedirectUri :: BS.ByteString,
    oidcSessionStore :: OIDC.SessionStore IO,
    oidcUserKey :: Vault.Key String
  }

data TokenData = TokenData
  { token_user :: Maybe String,
    token_clientId :: UUID.UUID,
    token_logins :: Int,
    token_access :: String
  }
  deriving (Generic, Show)

instance FromJSON TokenData

instance ToJSON TokenData

newtype IDToken = IDToken {email :: Text} deriving (Generic, Show)

instance FromJSON IDToken

instance ToJSON IDToken

data CookieData = CookieData
  { accessToken :: Maybe Text,
    username :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON CookieData

instance ToJSON CookieData

authMiddleware :: OIDCConfig -> Wai.Middleware
authMiddleware config app request respond = do
  manager <- liftIO newTlsManager
  provider <- OIDC.discover config.oidcDiscoveryUrl manager
  let oidc =
        OIDC.newOIDC provider
          & OIDC.setCredentials config.oidcClientId config.oidcClientSecret config.oidcRedirectUri
  case Wai.pathInfo request of
    -- Start the login flow
    -- FIXME: No PKCE?
    ["api", "oidc", "login"] -> do
      let scopes = ["sub", "name", "email"]
      let params = []
      authUrl <-
        OIDC.prepareAuthenticationRequestUrl config.oidcSessionStore oidc scopes params
      let cookieData = CookieData {accessToken = Nothing, username = Nothing}
      headers <- setSessionCookie cookieData [(hLocation, BS8.pack $ show authUrl)]
      respond $ Wai.responseLBS Http.status302 headers ""
    -- Handle the OIDC callback
    ["api", "oidc", "callback"] -> do
      let params =
            Wai.queryString request & \qs -> do
              code <- List.find ((== "code") . fst) qs >>= snd
              state <- List.find ((== "state") . fst) qs >>= snd
              pure (code, state)
      case params of
        Nothing -> respond $ Wai.responseLBS Http.status400 [] "No code"
        Just (code, state) -> do
          tokens <- OIDC.getValidTokens config.oidcSessionStore oidc manager state code :: IO (OIDC.Tokens IDToken)
          -- FIXME: Set the cookie
          let cookieData =
                CookieData
                  { accessToken = Just tokens.accessToken,
                    username = Just tokens.idToken.otherClaims.email
                  }
          headers <- setSessionCookie cookieData [(hLocation, config.oidcFrontendUri)]
          respond $ Wai.responseLBS Http.status302 headers ""
    ["api", "oidc", "userinfo"] -> do
      -- TODO: Check validity of access token by probing the userinfo endpoint
      tokenData <- readSessionCookie request.requestHeaders
      case tokenData >>= accessToken of
        Nothing -> respond $ Wai.responseLBS Http.status401 [] "No session"
        Just token -> do
          request' <- Http.parseRequest (Text.unpack $ config.oidcDiscoveryUrl <> "/userinfo")
          response <-
            Http.httpLbs
              ( request'
                  { Http.requestHeaders =
                      request'.requestHeaders
                        <> [ ("Authorization", "Bearer " <> Text.encodeUtf8 token)
                           ]
                  }
              )
              manager
          if Http.statusIsSuccessful (Http.responseStatus response)
            then respond $ Wai.responseLBS Http.status200 [] (Http.responseBody response)
            else respond $ Wai.responseLBS Http.status401 [] (Http.responseBody response)
    ["api", "oidc", "authorize"] -> do
      -- TODO: Check validity of access token by probing the userinfo endpoint
      tokenData <- readSessionCookie request.requestHeaders
      case tokenData >>= accessToken of
        Nothing -> respond $ Wai.responseLBS Http.status401 [] "No session"
        Just token -> do
          request' <- Http.parseRequest (Text.unpack $ config.oidcDiscoveryUrl <> "/authorize")
          response <-
            Http.httpLbs
              ( request'
                  { Http.requestHeaders =
                      request'.requestHeaders
                        <> [ ("Authorization", "Bearer " <> Text.encodeUtf8 token)
                           ]
                  }
              )
              manager
          if Http.statusIsSuccessful (Http.responseStatus response)
            then respond $ Wai.responseLBS Http.status200 [] (Http.responseBody response)
            else respond $ Wai.responseLBS Http.status401 [] (Http.responseBody response)
    ["api", "oidc", "logout"] -> do
      -- Clears the session cookie
      let cookieData = CookieData {accessToken = Nothing, username = Nothing}
      headers <- setSessionCookie cookieData [(hLocation, config.oidcFrontendUri)]
      respond $ Wai.responseLBS Http.status302 headers ""
    -- Direct other requests to the nested apps
    _ -> do
      tokenData <- readSessionCookie request.requestHeaders
      let newVault = case tokenData >>= username of
            Nothing -> request.vault
            Just uname -> Vault.insert config.oidcUserKey (Text.unpack uname) request.vault
      app request {vault = newVault} respond

readSessionCookie :: Http.RequestHeaders -> IO (Maybe CookieData)
readSessionCookie headers = do
  let mCookieHeader = List.find ((== hCookie) . fst) headers
      mCookieValue = Cookie.setCookieValue . Cookie.parseSetCookie . snd <$> mCookieHeader
  key <- Session.getDefaultKey
  case mCookieValue of
    Nothing -> do
      Log.err "FAILURE: No cookie"
      pure Nothing
    Just cookieValue -> case Session.decrypt key cookieValue of
      Nothing -> do
        Log.err "FAILURE: Unable to decode token"
        pure Nothing
      Just value -> parseTokenData value
  where
    parseTokenData :: BS.ByteString -> IO (Maybe CookieData)
    parseTokenData token = case JSON.eitherDecodeStrict token of
      Left err -> do
        Log.err $ "FAILURE: readSessionCookie: " <> err
        pure Nothing
      Right tokenData -> pure $ Just tokenData

setSessionCookie :: CookieData -> Http.ResponseHeaders -> IO Http.ResponseHeaders
setSessionCookie cookieData headers = do
  key <- Session.getDefaultKey
  cookieValue <- Session.encryptIO key (BS8.toStrict $ JSON.encode cookieData)
  let session =
        Cookie.defaultSetCookie
          { Cookie.setCookieName = "session",
            Cookie.setCookieValue = cookieValue,
            Cookie.setCookiePath = Just "/",
            Cookie.setCookieHttpOnly = True,
            Cookie.setCookieSameSite = Just Cookie.sameSiteStrict
          }
      cookieHeader = (hSetCookie, Cookie.renderSetCookieBS session)
  pure $ cookieHeader : headers

mkSessionStore :: IORef (Map BS.ByteString BS.ByteString) -> OIDC.SessionStore IO
mkSessionStore oidcSessionMap =
  OIDC.SessionStore
    { OIDC.sessionStoreGenerate = do
        UUID.toASCIIBytes <$> UUID.nextRandom,
      OIDC.sessionStoreGet = \key -> do
        IORef.atomicModifyIORef' oidcSessionMap $ \sessions' ->
          let mSession = Map.lookup key sessions'
           in (sessions', mSession),
      OIDC.sessionStoreSave = \state nonce -> do
        IORef.atomicModifyIORef' oidcSessionMap $ \sessions' ->
          (Map.insert state nonce sessions', ()),
      -- FIXME: Only delete the specific state
      OIDC.sessionStoreDelete = do
        IORef.atomicModifyIORef' oidcSessionMap $ const (Map.empty, ())
    }

mkOIDCConfig :: Vault.Key String -> IO OIDCConfig
mkOIDCConfig userKey = do
  discoveryUrl <- Text.pack <$> getEnv "OIDC_DISCOVERY_URL"
  clientId <- BS8.pack <$> getEnv "OIDC_CLIENT_ID"
  clientSecret <- BS8.pack <$> getEnv "OIDC_CLIENT_SECRET"
  frontendBaseURL <- BS8.pack <$> getEnv "FRONTEND_BASE_URL"
  redirectURI <-  BS8.pack . (<> "/api/oidc/callback") <$> getEnv "BACKEND_BASE_URL"
  oidcSessionMap <- IORef.newIORef Map.empty
  pure
    OIDCConfig
      { oidcDiscoveryUrl = discoveryUrl,
        oidcClientId = clientId,
        oidcClientSecret = clientSecret,
        oidcFrontendUri = frontendBaseURL,
        oidcRedirectUri = redirectURI,
        oidcSessionStore = mkSessionStore oidcSessionMap,
        oidcUserKey = userKey
      }
