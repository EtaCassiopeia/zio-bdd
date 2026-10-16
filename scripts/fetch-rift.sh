#!/usr/bin/env bash
#
# fetch-rift.sh — download a released rift engine for the Plane B `rift-external` lane (#361).
#
# The only engine coupling Plane B has: a released binary at a pinned version, never a source build.
#
# Usage:  eval "$(scripts/fetch-rift.sh)"
#
# On success the ONLY thing written to stdout is one line:
#   export RIFT_BUILD_DIR=<dir holding the rift binary>
# All diagnostics go to stderr.
#
# Environment:
#   RIFT_BUILD_DIR    if set and it holds an executable `rift`, it is echoed back unchanged (local builds).
#   RIFT_VERSION      the rift release to fetch (default below — bump it to move the lane to a new engine).
#   RIFT_BUILD_CACHE  cache root (default ~/.cache/zio-bdd-rift); a release already there is reused.
set -euo pipefail

REPO="achird-labs/rift"
RIFT_VERSION="${RIFT_VERSION:-0.21.0}"
CACHE_ROOT="${RIFT_BUILD_CACHE:-$HOME/.cache/zio-bdd-rift}"

log() { echo "fetch-rift: $*" >&2; }
emit() { echo "export RIFT_BUILD_DIR=\"$1\""; }

if [ -n "${RIFT_BUILD_DIR:-}" ]; then
  [ -x "$RIFT_BUILD_DIR/rift" ] || { log "RIFT_BUILD_DIR=$RIFT_BUILD_DIR has no executable rift"; exit 1; }
  log "RIFT_BUILD_DIR set — using $RIFT_BUILD_DIR"
  emit "$RIFT_BUILD_DIR"
  exit 0
fi

case "$(uname -s)-$(uname -m)" in
  Darwin-arm64) TRIPLE="aarch64-apple-darwin" ;;
  Darwin-x86_64) TRIPLE="x86_64-apple-darwin" ;;
  Linux-aarch64) TRIPLE="aarch64-unknown-linux-gnu" ;;
  Linux-x86_64) TRIPLE="x86_64-unknown-linux-gnu" ;;
  *) log "unsupported platform: $(uname -s)/$(uname -m)"; exit 1 ;;
esac

TARGET_DIR="$CACHE_ROOT/rift-v$RIFT_VERSION-$TRIPLE"
BUILD_DIR="$TARGET_DIR/bin"
if [ -x "$BUILD_DIR/rift" ]; then
  log "cache hit for rift v$RIFT_VERSION ($TRIPLE)"
  emit "$BUILD_DIR"
  exit 0
fi

mkdir -p "$CACHE_ROOT"
ASSET="rift-v$RIFT_VERSION-$TRIPLE.tar.gz"
WORK="$(mktemp -d "$CACHE_ROOT/.download-XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

log "fetching $ASSET"
if command -v gh >/dev/null 2>&1; then
  gh release download "v$RIFT_VERSION" --repo "$REPO" --pattern "$ASSET*" --dir "$WORK" --clobber >&2
else
  BASE_URL="https://github.com/$REPO/releases/download/v$RIFT_VERSION"
  curl -fsSL -o "$WORK/$ASSET" "$BASE_URL/$ASSET"
  curl -fsSL -o "$WORK/$ASSET.sha256" "$BASE_URL/$ASSET.sha256"
fi
[ -f "$WORK/$ASSET" ] && [ -f "$WORK/$ASSET.sha256" ] || { log "download incomplete: $ASSET"; exit 1; }

log "verifying sha256"
if command -v sha256sum >/dev/null 2>&1; then
  (cd "$WORK" && sha256sum -c "$ASSET.sha256") >&2
else
  (cd "$WORK" && shasum -a 256 -c "$ASSET.sha256") >&2
fi

rm -rf "$TARGET_DIR"
tar -xzf "$WORK/$ASSET" -C "$CACHE_ROOT"
[ -f "$BUILD_DIR/rift" ] || { log "archive did not contain $BUILD_DIR/rift"; exit 1; }
chmod +x "$BUILD_DIR/rift"
log "rift v$RIFT_VERSION ready: $("$BUILD_DIR/rift" --version 2>&1 | head -1)"
emit "$BUILD_DIR"
