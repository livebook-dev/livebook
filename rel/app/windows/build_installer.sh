#!/bin/sh
set -euo pipefail

. `dirname $0`/env.sh

rm -rf $target_dir
dotnet publish $build_args
(cd ../../.. && mix release app --overwrite --path=${target_dir}/rel)

vc_redist_path="bin/vc_redist.x64.exe"
if [ ! -f $vc_redist_path ]; then
  url="https://aka.ms/vs/17/release/vc_redist.x64.exe"
  echo "downloading $url"
  curl -L --fail --output $vc_redist_path $url
fi

makensis //DERTS_VERSION=`elixir -e "IO.puts :erlang.system_info(:version)"` Installer.nsi
