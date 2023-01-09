#!/bin/sh
set -euo pipefail

. `dirname $0`/env.sh

rm -rf $target_dir
dotnet build $build_args
(cd ../../.. && mix release app --overwrite --path=${target_dir}/rel)
