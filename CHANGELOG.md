# Changelog

## 0.2.0.0 — 2026-02-22

- New module: `NovaCache.Validate` — pure protocol validation layer
- `ValidationError` sum type with 10 constructors covering sizes, store paths,
  hash formats, content hashes, and Ed25519 signatures
- `validateNarInfo` — field semantic validation (non-negative sizes, parseable
  store paths/hashes/references), collects all errors instead of short-circuiting
- `validateNarHash` / `validateFileHash` — SHA-256 content hash verification
  against declared narinfo values
- `validateSignature` — Ed25519 signature verification against a trusted public
  key (at-least-one semantics, matching Nix behaviour)
- `validateFull` — composes all four stages, collecting errors across all
- 17 new tests (74 total across 9 groups)
- No new dependencies

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
