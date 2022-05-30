#!/bin/bash
#
# Usage:
#
#     $ sh .github/scripts/app/build_windows.sh
#     $ wscript _build/app_prod/Livebook-win/LivebookLauncher.vbs
#     $ start livebook://github.com/livebook-dev/livebook/blob/main/test/support/notebooks/basic.livemd
#     $ start ./test/support/notebooks/basic.livemd
set -e

MIX_ENV=prod MIX_TARGET=app mix release app --overwrite
