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
    getCacheInfo,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
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
defaultPriority :: Int
defaultPriority = 30

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
readNarInfo :: FileStore -> Text -> IO (Maybe ByteString)
readNarInfo fs hashKey =
  readIfExists (fsRoot fs </> narinfoSubdir </> T.unpack hashKey)

-- | Write a narinfo by its store path hash.
writeNarInfo :: FileStore -> Text -> ByteString -> IO ()
writeNarInfo fs hashKey =
  BS.writeFile (fsRoot fs </> narinfoSubdir </> T.unpack hashKey)

-- ---------------------------------------------------------------------------
-- NAR operations
-- ---------------------------------------------------------------------------

-- | Read a NAR file by its filename. Returns 'Nothing' if absent.
readNar :: FileStore -> Text -> IO (Maybe ByteString)
readNar fs fileName =
  readIfExists (fsRoot fs </> narSubdir </> T.unpack fileName)

-- | Write a NAR file by its filename.
writeNar :: FileStore -> Text -> ByteString -> IO ()
writeNar fs fileName =
  BS.writeFile (fsRoot fs </> narSubdir </> T.unpack fileName)

-- ---------------------------------------------------------------------------
-- Cache metadata
-- ---------------------------------------------------------------------------

-- | Cache metadata: (storeDir, wantMassQuery, priority).
getCacheInfo :: FileStore -> (Text, Bool, Int)
getCacheInfo fs = (fsStoreDir fs, True, fsPriority fs)

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
