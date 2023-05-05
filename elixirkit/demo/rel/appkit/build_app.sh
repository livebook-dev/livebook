#!/bin/sh
set -euo pipefail

export MIX_ENV=prod
export ELIXIRKIT_APP_NAME=Demo
export ELIXIRKIT_PROJECT_DIR=$PWD/../../..

. ../../../elixirkit_swift/Scripts/build_macos_app.sh
