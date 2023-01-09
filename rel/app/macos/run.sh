#!/bin/sh
set -euo pipefail

. `dirname $0`/build_app.sh
open -W --stdout `tty` --stderr `tty` $app_dir
