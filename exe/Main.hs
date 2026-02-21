module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GBNix.Store (FileStore, getCacheInfo, newFileStore, readNar, readNarInfo, writeNar, writeNarInfo)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Application, pathInfo, requestMethod, responseLBS, strictRequestBody)
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (getArgs, lookupEnv)

-- | Default server port.
defaultPort :: Int
defaultPort = 5000

-- | Default store directory.
defaultStoreRoot :: FilePath
defaultStoreRoot = "./nix-cache"

main :: IO ()
main = do
  args <- getArgs
  portEnv <- lookupEnv "PORT"
  storeEnv <- lookupEnv "NIX_CACHE_DIR"

  let port = case args of
        ("--port" : p : _) -> read p
        _ -> maybe defaultPort read portEnv
      storeRoot = case args of
        ("--store" : s : _) -> s
        _ -> fromMaybe defaultStoreRoot storeEnv

  store <- newFileStore storeRoot
  putStrLn ("gb-nix-cache-server listening on port " ++ show port)
  putStrLn ("store root: " ++ storeRoot)
  Warp.run port (app store)

-- | WAI application implementing the Nix binary cache HTTP protocol.
app :: FileStore -> Application
app store req respond = case (requestMethod req, pathInfo req) of
  -- GET /nix-cache-info
  ("GET", ["nix-cache-info"]) ->
    respond (responseLBS HTTP.status200 textHeaders (BL.fromStrict (renderCacheInfo store)))
  -- GET /<hash>.narinfo
  ("GET", [hashNarinfo])
    | Just hashKey <- T.stripSuffix ".narinfo" hashNarinfo -> do
        result <- readNarInfo store hashKey
        case result of
          Just content ->
            respond (responseLBS HTTP.status200 textHeaders (BL.fromStrict content))
          Nothing ->
            respond (responseLBS HTTP.status404 textHeaders "not found")
  -- GET /nar/<file>
  ("GET", ["nar", fileName]) -> do
    result <- readNar store fileName
    case result of
      Just content ->
        respond (responseLBS HTTP.status200 octetHeaders (BL.fromStrict content))
      Nothing ->
        respond (responseLBS HTTP.status404 textHeaders "not found")
  -- PUT /<hash>.narinfo
  ("PUT", [hashNarinfo])
    | Just hashKey <- T.stripSuffix ".narinfo" hashNarinfo -> do
        body <- BL.toStrict <$> strictRequestBody req
        writeNarInfo store hashKey body
        respond (responseLBS HTTP.status200 textHeaders "ok")
  -- PUT /nar/<file>
  ("PUT", ["nar", fileName]) -> do
    body <- BL.toStrict <$> strictRequestBody req
    writeNar store fileName body
    respond (responseLBS HTTP.status200 textHeaders "ok")
  -- Fallback
  _ ->
    respond (responseLBS HTTP.status404 textHeaders "not found")

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

-- | Content-Type: application/octet-stream headers.
octetHeaders :: HTTP.ResponseHeaders
octetHeaders = [(HTTP.hContentType, "application/octet-stream")]
