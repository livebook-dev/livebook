#!/bin/sh
set -euo pipefail

export MIX_ENV=prod
export MIX_TARGET=app
export ELIXIRKIT_APP_NAME=Livebook
export ELIXIRKIT_PROJECT_DIR=$PWD/../../..
export ELIXIRKIT_RELEASE_NAME=app

configuration=${ELIXIRKIT_CONFIGURATION:-Debug}
target_dir="$PWD/bin/${ELIXIRKIT_APP_NAME}-$configuration"
build_args="--configuration $configuration"
