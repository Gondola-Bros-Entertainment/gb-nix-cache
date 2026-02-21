module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Application, Request, Response, ResponseReceived, pathInfo, requestHeaders, requestMethod, responseLBS, strictRequestBody)
import qualified Network.Wai.Handler.Warp as Warp
import NovaCache.NarInfo (NarInfo (..), parseNarInfo, renderNarInfo)
import NovaCache.Signing (SecretKey, parseSecretKey, sign)
import NovaCache.Store (FileStore, getCacheInfo, newFileStore, readNar, readNarInfo, writeNar, writeNarInfo)
import System.Environment (getArgs, lookupEnv)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Default server port.
defaultPort :: Int
defaultPort = 5000

-- | Default store directory.
defaultStoreRoot :: FilePath
defaultStoreRoot = "./nix-cache"

-- | Environment variable for the write API key.
apiKeyEnvVar :: String
apiKeyEnvVar = "CACHE_API_KEY"

-- | Environment variable for the signing key file path.
signingKeyEnvVar :: String
signingKeyEnvVar = "SIGNING_KEY_FILE"

-- ---------------------------------------------------------------------------
-- Server configuration
-- ---------------------------------------------------------------------------

-- | Runtime server configuration.
data Config = Config
  { cfgStore :: !FileStore,
    cfgApiKey :: !(Maybe BS.ByteString),
    cfgSigningKey :: !(Maybe SecretKey)
  }

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  portEnv <- lookupEnv "PORT"
  storeEnv <- lookupEnv "NIX_CACHE_DIR"
  apiKeyEnv <- lookupEnv apiKeyEnvVar
  sigKeyPath <- lookupEnv signingKeyEnvVar

  let port = case args of
        ("--port" : p : _) -> read p
        _ -> maybe defaultPort read portEnv
      storeRoot = case args of
        ("--store" : s : _) -> s
        _ -> fromMaybe defaultStoreRoot storeEnv

  store <- newFileStore storeRoot
  sigKey <- loadSigningKey sigKeyPath

  let cfg =
        Config
          { cfgStore = store,
            cfgApiKey = TE.encodeUtf8 . T.pack <$> apiKeyEnv,
            cfgSigningKey = sigKey
          }

  putStrLn ("nova-cache-server listening on port " ++ show port)
  putStrLn ("store root: " ++ storeRoot)
  putStrLn ("signing: " ++ maybe "disabled" (const "enabled") sigKey)
  putStrLn ("write auth: " ++ maybe "disabled (open writes!)" (const "enabled") (cfgApiKey cfg))
  Warp.run port (app cfg)

-- | Load a signing key from a file, if configured.
loadSigningKey :: Maybe FilePath -> IO (Maybe SecretKey)
loadSigningKey Nothing = pure Nothing
loadSigningKey (Just path) = do
  contents <- T.strip . TE.decodeUtf8 <$> BS.readFile path
  case parseSecretKey contents of
    Left err -> do
      putStrLn ("WARNING: failed to load signing key: " ++ err)
      pure Nothing
    Right sk -> pure (Just sk)

-- ---------------------------------------------------------------------------
-- WAI application
-- ---------------------------------------------------------------------------

-- | WAI application implementing the Nix binary cache HTTP protocol.
app :: Config -> Application
app cfg req respond = case (requestMethod req, pathInfo req) of
  -- GET /nix-cache-info
  ("GET", ["nix-cache-info"]) ->
    respond (responseLBS HTTP.status200 textHeaders (BL.fromStrict (renderCacheInfo (cfgStore cfg))))
  -- GET /<hash>.narinfo
  ("GET", [hashNarinfo])
    | Just hashKey <- T.stripSuffix ".narinfo" hashNarinfo -> do
        result <- readNarInfo (cfgStore cfg) hashKey
        case result of
          Just content ->
            respond (responseLBS HTTP.status200 narInfoHeaders (BL.fromStrict content))
          Nothing ->
            respond (responseLBS HTTP.status404 textHeaders "not found")
  -- GET /nar/<file>
  ("GET", ["nar", fileName]) -> do
    result <- readNar (cfgStore cfg) fileName
    case result of
      Just content ->
        respond (responseLBS HTTP.status200 octetHeaders (BL.fromStrict content))
      Nothing ->
        respond (responseLBS HTTP.status404 textHeaders "not found")
  -- PUT /<hash>.narinfo (auth required)
  ("PUT", [hashNarinfo])
    | Just hashKey <- T.stripSuffix ".narinfo" hashNarinfo ->
        requireAuth cfg req respond $ do
          body <- BL.toStrict <$> strictRequestBody req
          let signed = signNarInfo (cfgSigningKey cfg) body
          writeNarInfo (cfgStore cfg) hashKey signed
          respond (responseLBS HTTP.status200 textHeaders "ok")
  -- PUT /nar/<file> (auth required)
  ("PUT", ["nar", fileName]) ->
    requireAuth cfg req respond $ do
      body <- BL.toStrict <$> strictRequestBody req
      writeNar (cfgStore cfg) fileName body
      respond (responseLBS HTTP.status200 textHeaders "ok")
  -- Fallback
  _ ->
    respond (responseLBS HTTP.status404 textHeaders "not found")

-- ---------------------------------------------------------------------------
-- Auth
-- ---------------------------------------------------------------------------

-- | Gate a handler behind API key authentication.
--
-- If no key is configured, all writes are permitted (open mode).
-- Otherwise the request must carry @Authorization: Bearer \<key\>@.
requireAuth :: Config -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -> IO ResponseReceived
requireAuth cfg req respond action = case cfgApiKey cfg of
  Nothing -> action
  Just expected ->
    let provided = lookup HTTP.hAuthorization (requestHeaders req)
     in if provided == Just ("Bearer " <> expected)
          then action
          else respond (responseLBS HTTP.status401 textHeaders "unauthorized")

-- ---------------------------------------------------------------------------
-- Signing
-- ---------------------------------------------------------------------------

-- | Sign a narinfo body if a signing key is configured.
signNarInfo :: Maybe SecretKey -> BS.ByteString -> BS.ByteString
signNarInfo Nothing body = body
signNarInfo (Just sk) body = case parseNarInfo (TE.decodeUtf8 body) of
  Left _ -> body
  Right ni -> case sign sk ni of
    Left _ -> body
    Right sig ->
      let signed = ni {niSigs = niSigs ni ++ [sig]}
       in TE.encodeUtf8 (renderNarInfo signed)

-- ---------------------------------------------------------------------------
-- Response helpers
-- ---------------------------------------------------------------------------

-- | Render the nix-cache-info response body.
renderCacheInfo :: FileStore -> BS.ByteString
renderCacheInfo store =
  let (storeDir, wantMassQuery, priority) = getCacheInfo store
   in TE.encodeUtf8 $
        T.unlines
          [ "StoreDir: " <> storeDir,
            "WantMassQuery: " <> boolText wantMassQuery,
            "Priority: " <> T.pack (show priority)
          ]

-- | Render a Bool as @1@ or @0@.
boolText :: Bool -> Text
boolText True = "1"
boolText False = "0"

-- | Content-Type: text/plain headers.
textHeaders :: HTTP.ResponseHeaders
textHeaders = [(HTTP.hContentType, "text/plain")]

-- | Content-Type: application/x-nix-narinfo headers.
narInfoHeaders :: HTTP.ResponseHeaders
narInfoHeaders = [(HTTP.hContentType, "text/x-nix-narinfo")]

-- | Content-Type: application/octet-stream headers.
octetHeaders :: HTTP.ResponseHeaders
octetHeaders = [(HTTP.hContentType, "application/octet-stream")]
