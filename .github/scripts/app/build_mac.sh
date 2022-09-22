#!/bin/bash
#
# Usage:
#
#     $ sh .github/scripts/app/build_mac.sh
#     $ open _build/app_prod/Livebook.app
#     $ open livebook://github.com/livebook-dev/livebook/blob/main/test/support/notebooks/basic.livemd
#     $ open ./test/support/notebooks/basic.livemd
#
# Debugging:
#
#     $ cd _build/app_prod/Livebook.app/Contents/Resources/
#     $ lldb ../MacOS/LivebookLauncher
#     (lldb) $ run
set -e

. .github/scripts/app/bootstrap_mac.sh
mix local.hex --force --if-missing
mix local.rebar --force --if-missing
MIX_ENV=prod MIX_TARGET=app mix deps.get --only prod
MIX_ENV=prod MIX_TARGET=app mix release app --overwrite
