#!/bin/sh
set -euo pipefail

. `dirname $0`/build_app.sh
. ../../../elixirkit_swift/Scripts/build_macos_dmg.sh
