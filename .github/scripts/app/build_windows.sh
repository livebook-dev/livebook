#!/bin/bash
#
# Usage:
#
#     $ sh .github/scripts/app/build_windows.sh
#     $ rel/app/windows/bin/LivebookInstall.exe
#     $ start livebook://github.com/livebook-dev/livebook/blob/main/test/support/notebooks/basic.livemd
#     $ start ./test/support/notebooks/basic.livemd
#
# Note: This script builds the Windows installer. If you just want to test the Windows app locally, run:
#
#     $ cd rel/app/windows && ./run.sh
#
# See rel/app/windows/README.md for more information.
set -e

mix local.hex --force --if-missing
mix local.rebar --force --if-missing

export MIX_ENV=prod
export MIX_TARGET=app
export ELIXIRKIT_CONFIGURATION=Release

mix deps.get --only prod

cd rel/app/windows
./build_installer.sh
