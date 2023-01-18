#!/bin/sh
set -euo pipefail

swift build
target_dir=`swift build --show-bin-path`
(cd ../.. && mix release --overwrite --path=$target_dir/rel)
$target_dir/Demo
