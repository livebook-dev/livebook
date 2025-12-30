#!/usr/bin/env sh
set -eu

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
LOG_DIR="$ROOT_DIR/logs"
TAURI_LOG="$LOG_DIR/tauri.out"
TAURI_PID_FILE="$LOG_DIR/tauri.pid"

mkdir -p "$LOG_DIR"
: > "$TAURI_LOG"
rm -f "$TAURI_PID_FILE"

(
  cd "$ROOT_DIR/src-tauri"
  cargo build
)

(
  cd "$ROOT_DIR/src-tauri"
  RUST_LOG=error target/debug/livebook > "$TAURI_LOG" 2>&1 &
  echo $! > "$TAURI_PID_FILE"
)

TAURI_PID="$(cat "$TAURI_PID_FILE")"

i=0
while ! grep -q "Application running" "$TAURI_LOG"; do
  if ! kill -0 "$TAURI_PID" 2>/dev/null; then
    echo "tauri exited before elixir started" >&2
    exit 1
  fi
  if [ $i -gt 300 ]; then
    echo "timeout waiting for elixir to start" >&2
    exit 1
  fi
  sleep 0.1
  i=$((i + 1))
done

MIX_PID="$(pgrep -P "$TAURI_PID" | head -n1 || true)"
if [ -z "$MIX_PID" ]; then
  echo "failed to find mix process" >&2
  exit 1
fi

ELIXIR_PID="$(pgrep -P "$MIX_PID" | head -n1 || true)"
if [ -z "$ELIXIR_PID" ]; then
  ELIXIR_PID="$MIX_PID"
fi

kill "$TAURI_PID" 2>/dev/null || true

j=0
while kill -0 "$TAURI_PID" 2>/dev/null; do
  if [ $j -gt 200 ]; then
    echo "tauri process still running" >&2
    exit 1
  fi
  sleep 0.05
  j=$((j + 1))
done

k=0
while kill -0 "$MIX_PID" 2>/dev/null || kill -0 "$ELIXIR_PID" 2>/dev/null; do
  if [ $k -gt 200 ]; then
    echo "elixir process still running after tauri exit" >&2
    exit 1
  fi
  sleep 0.05
  k=$((k + 1))
done

echo "ok"
