#!/bin/sh
set -euo pipefail

. `dirname $0`/build_app.sh

# Deregister /Applications/Livebook.app so that the "dev version" will handle .livemd and livebook://
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister \
  -u /Applications/Livebook.app \
  || true

/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister \
  "$app_dir"

open -W --stdout `tty` --stderr `tty` "$app_dir"
