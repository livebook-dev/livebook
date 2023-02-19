#!/bin/sh
set -euo pipefail

export ELIXIRKIT_APP_NAME=Demo
export ELIXIRKIT_PROJECT_DIR=$PWD/../..

. ../../../../elixirkit/elixirkit_swift/Scripts/build_macos_app.sh
open -W --stdout `tty` --stderr `tty` .build/Demo.app
