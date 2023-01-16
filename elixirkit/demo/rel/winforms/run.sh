#!/bin/sh
set -euo pipefail

dotnet build
target_dir="$PWD/bin/Debug/net6.0-windows"
(cd ../.. && mix release --overwrite --path=${target_dir}/rel)
dotnet run --no-build
