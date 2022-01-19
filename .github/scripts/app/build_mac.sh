#!/bin/bash
# Usage:
#
#     $ sh .github/scripts/app/build_mac.sh
#     $ open _build/app_prod/rel/Livebook.app
#     $ open livebook://github.com/livebook-dev/livebook/blob/main/test/support/notebooks/basic.livemd
#     $ open ./test/support/notebooks/basic.livemd
set -e

sh .github/scripts/app/bootstrap_mac.sh
. tmp/bootstrap_env.sh

# If CODESIGN_IDENITY is set, let's build the .dmg which would also notarize it.
# Otherwise, let's build just the .app.
if [ -n "$CODESIGN_IDENTITY" ]; then
  MIX_ENV=prod MIX_TARGET=app mix release mac_app_dmg --overwrite
else
  MIX_ENV=prod MIX_TARGET=app mix release mac_app --overwrite
fi
