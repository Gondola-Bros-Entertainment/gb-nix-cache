# Changelog

## 0.1.0.0 — 2026-02-21

- Initial release (renamed from gb-nix-cache)
- Nix-base32 encoding/decoding
- SHA-256 hashing with Nix hash formatting
- Store path parsing and rendering
- NAR binary format serialization/deserialization
- NarInfo text format parsing/rendering
- Ed25519 signing and verification
- xz compression/decompression
- Filesystem storage backend
- Optional WAI cache server (behind `server` flag)
- Path traversal protection on all store operations
- Total port parsing (no partial `read`)
- 54 tests across 8 modules
