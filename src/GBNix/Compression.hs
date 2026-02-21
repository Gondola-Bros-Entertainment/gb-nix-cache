-- | xz compression and decompression for NAR files.
--
-- Thin wrappers around the @lzma@ package, converting between strict
-- 'ByteString' and the underlying lazy interface.
module GBNix.Compression
  ( compressXz,
    decompressXz,
  )
where

import qualified Codec.Compression.Lzma as Lzma
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL

-- | Compress a strict 'ByteString' with xz.
compressXz :: ByteString -> ByteString
compressXz = BL.toStrict . Lzma.compress . BL.fromStrict

-- | Decompress an xz-compressed strict 'ByteString'.
decompressXz :: ByteString -> ByteString
decompressXz = BL.toStrict . Lzma.decompress . BL.fromStrict
