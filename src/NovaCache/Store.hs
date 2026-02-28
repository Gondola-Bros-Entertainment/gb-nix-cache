-- | Filesystem storage backend for a Nix binary cache.
--
-- Stores narinfo and NAR files in a directory tree:
--
-- @
-- \<root\>\/narinfo\/\<hash\>   -- narinfo files by store path hash
-- \<root\>\/nar\/\<filename\>   -- compressed NAR files
-- @
module NovaCache.Store
  ( FileStore (..),
    newFileStore,
    readNarInfo,
    writeNarInfo,
    readNar,
    writeNar,
    listNarInfoHashes,
    getCacheInfo,
    sanitizePath,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    listDirectory,
  )
import System.FilePath ((</>))

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Filesystem-backed cache store configuration.
data FileStore = FileStore
  { fsRoot :: !FilePath,
    fsStoreDir :: !Text,
    fsPriority :: !Int
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Subdirectory for narinfo files.
narinfoSubdir :: String
narinfoSubdir = "narinfo"

-- | Subdirectory for NAR files.
narSubdir :: String
narSubdir = "nar"

-- | Default cache priority (lower = preferred by Nix).
-- Community caches should use a higher number than cache.nixos.org (40)
-- so the official cache is preferred and we serve as a fallback.
defaultPriority :: Int
defaultPriority = 50

-- | Default Nix store directory.
defaultStoreDir :: Text
defaultStoreDir = "/nix/store"

-- ---------------------------------------------------------------------------
-- Initialization
-- ---------------------------------------------------------------------------

-- | Create a 'FileStore' rooted at the given directory.
--
-- Ensures the @narinfo@ and @nar@ subdirectories exist.
newFileStore :: FilePath -> IO FileStore
newFileStore root = do
  createDirectoryIfMissing True (root </> narinfoSubdir)
  createDirectoryIfMissing True (root </> narSubdir)
  pure
    FileStore
      { fsRoot = root,
        fsStoreDir = defaultStoreDir,
        fsPriority = defaultPriority
      }

-- ---------------------------------------------------------------------------
-- NarInfo operations
-- ---------------------------------------------------------------------------

-- | Read a narinfo by its store path hash. Returns 'Nothing' if absent.
--
-- Returns 'Nothing' for path components containing traversal sequences.
readNarInfo :: FileStore -> Text -> IO (Maybe ByteString)
readNarInfo fs hashKey = case sanitizePath hashKey of
  Nothing -> pure Nothing
  Just safe -> readIfExists (fsRoot fs </> narinfoSubdir </> safe)

-- | Write a narinfo by its store path hash.
--
-- Returns 'False' for path components containing traversal sequences.
writeNarInfo :: FileStore -> Text -> ByteString -> IO Bool
writeNarInfo fs hashKey body = case sanitizePath hashKey of
  Nothing -> pure False
  Just safe -> BS.writeFile (fsRoot fs </> narinfoSubdir </> safe) body >> pure True

-- ---------------------------------------------------------------------------
-- NAR operations
-- ---------------------------------------------------------------------------

-- | Read a NAR file by its filename. Returns 'Nothing' if absent.
--
-- Returns 'Nothing' for path components containing traversal sequences.
readNar :: FileStore -> Text -> IO (Maybe ByteString)
readNar fs fileName = case sanitizePath fileName of
  Nothing -> pure Nothing
  Just safe -> readIfExists (fsRoot fs </> narSubdir </> safe)

-- | Write a NAR file by its filename.
--
-- Returns 'False' for path components containing traversal sequences.
writeNar :: FileStore -> Text -> ByteString -> IO Bool
writeNar fs fileName body = case sanitizePath fileName of
  Nothing -> pure False
  Just safe -> BS.writeFile (fsRoot fs </> narSubdir </> safe) body >> pure True

-- ---------------------------------------------------------------------------
-- Listing
-- ---------------------------------------------------------------------------

-- | List all narinfo hashes currently stored.
--
-- Returns the filenames in the @narinfo/@ subdirectory as 'Text' values.
listNarInfoHashes :: FileStore -> IO [Text]
listNarInfoHashes fs = map T.pack <$> listDirectory (fsRoot fs </> narinfoSubdir)

-- ---------------------------------------------------------------------------
-- Cache metadata
-- ---------------------------------------------------------------------------

-- | Cache metadata: (storeDir, wantMassQuery, priority).
getCacheInfo :: FileStore -> (Text, Bool, Int)
getCacheInfo fs = (fsStoreDir fs, True, fsPriority fs)

-- ---------------------------------------------------------------------------
-- Path sanitization
-- ---------------------------------------------------------------------------

-- | Validate a path component for safe filesystem use.
--
-- Rejects components containing directory separators (@\/@, @\\@),
-- traversal sequences (@..@), or empty strings. Returns 'Just' the
-- sanitized 'FilePath' on success, 'Nothing' on rejection.
sanitizePath :: Text -> Maybe FilePath
sanitizePath txt
  | T.null txt = Nothing
  | T.any isPathSeparator txt = Nothing
  | txt == ".." = Nothing
  | otherwise = Just (T.unpack txt)
  where
    isPathSeparator c = c == '/' || c == '\\'

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

-- | Read a file if it exists, returning 'Nothing' otherwise.
readIfExists :: FilePath -> IO (Maybe ByteString)
readIfExists path = do
  exists <- doesFileExist path
  if exists
    then Just <$> BS.readFile path
    else pure Nothing
