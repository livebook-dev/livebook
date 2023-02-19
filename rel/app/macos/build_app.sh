#!/bin/sh
set -euo pipefail

export MIX_TARGET=app
export MIX_ENV=prod
export ELIXIRKIT_APP_NAME=Livebook
export ELIXIRKIT_PROJECT_DIR=$PWD/../../..
export ELIXIRKIT_RELEASE_NAME=app

. ../../../elixirkit/elixirkit_swift/Scripts/build_macos_app.sh
