-- | SHA-256 hashing with Nix-specific formatting.
--
-- Nix represents content hashes as @sha256:\<nix-base32\>@. This module
-- provides the bridge between raw cryptographic hashing and the Nix
-- string representation, composing 'Crypto.Hash' with 'NovaCache.Base32'.
module NovaCache.Hash
  ( NixHash (..),
    hashBytes,
    hashFile,
    formatNixHash,
    parseNixHash,
  )
where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified NovaCache.Base32 as Base32

-- | A SHA-256 hash as raw bytes (always exactly 32 bytes).
newtype NixHash = NixHash ByteString
  deriving (Eq, Ord, Show)

-- | Size of a SHA-256 digest in bytes.
sha256DigestSize :: Int
sha256DigestSize = 32

-- | Hash algorithm prefix used in Nix hash strings.
sha256Prefix :: Text
sha256Prefix = "sha256:"

-- | Compute the SHA-256 hash of a strict 'ByteString'.
hashBytes :: ByteString -> NixHash
hashBytes = NixHash . convert . sha256

-- | Compute the SHA-256 hash of a file's contents.
hashFile :: FilePath -> IO NixHash
hashFile path = hashBytes <$> BS.readFile path

-- | Format a 'NixHash' as @sha256:\<nix-base32\>@.
formatNixHash :: NixHash -> Text
formatNixHash (NixHash raw) = sha256Prefix <> Base32.encode raw

-- | Parse a @sha256:\<nix-base32\>@ string back to a 'NixHash'.
--
-- Validates both the prefix and the decoded length.
parseNixHash :: Text -> Either String NixHash
parseNixHash txt = case T.stripPrefix sha256Prefix txt of
  Nothing -> Left ("expected " ++ T.unpack sha256Prefix ++ " prefix, got: " ++ T.unpack txt)
  Just encoded -> do
    decoded <- Base32.decode encoded
    if BS.length decoded == sha256DigestSize
      then Right (NixHash decoded)
      else Left (digestSizeError (BS.length decoded))

-- | Internal: compute SHA-256 digest.
sha256 :: ByteString -> Digest SHA256
sha256 = hash

-- | Error message for wrong digest size.
digestSizeError :: Int -> String
digestSizeError actual =
  "expected " ++ show sha256DigestSize ++ "-byte SHA-256 digest, got " ++ show actual ++ " bytes"
