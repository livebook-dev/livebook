#!/usr/bin/env sh
set -eu

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
RELEASE_DIR="$ROOT_DIR/resources/rel"

(
  cd "$ROOT_DIR/src-tauri"
  cargo build
)

if [ ! -x "$RELEASE_DIR/bin/app" ]; then
  echo "missing release script: $RELEASE_DIR/bin/app" >&2
  exit 1
fi

if ! ls "$RELEASE_DIR"/erts-* >/dev/null 2>&1; then
  echo "missing erts directory in $RELEASE_DIR" >&2
  exit 1
fi

if [ ! -d "$RELEASE_DIR/releases" ]; then
  echo "missing releases directory in $RELEASE_DIR" >&2
  exit 1
fi

echo "ok"
