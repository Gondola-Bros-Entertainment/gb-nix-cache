-- | xz compression and decompression for NAR files.
--
-- Thin wrappers around the @lzma@ package, converting between strict
-- 'ByteString' and the underlying lazy interface.
module NovaCache.Compression
  ( compressXz,
    decompressXz,
  )
where

import qualified Codec.Compression.Lzma as Lzma
import Control.Exception (SomeException, evaluate, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL

-- | Compress a strict 'ByteString' with xz.
compressXz :: ByteString -> ByteString
compressXz = BL.toStrict . Lzma.compress . BL.fromStrict

-- | Decompress an xz-compressed strict 'ByteString'.
--
-- Returns 'Left' with an error message if the input is not valid xz data.
decompressXz :: ByteString -> IO (Either String ByteString)
decompressXz bs = do
  result <- try (evaluate (BL.toStrict (Lzma.decompress (BL.fromStrict bs))))
  pure $ case result of
    Left err -> Left ("xz decompression failed: " ++ show (err :: SomeException))
    Right decompressed -> Right decompressed
