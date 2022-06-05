#!/bin/bash
#
# Usage:
#
#     $ sh .github/scripts/app/build_mac.sh
#     $ open _build/app_prod/Livebook.app
#     $ open livebook://github.com/livebook-dev/livebook/blob/main/test/support/notebooks/basic.livemd
#     $ open ./test/support/notebooks/basic.livemd
set -e

sh .github/scripts/app/bootstrap_mac.sh
. tmp/bootstrap_env.sh
MIX_ENV=prod MIX_TARGET=app mix release app --overwrite
