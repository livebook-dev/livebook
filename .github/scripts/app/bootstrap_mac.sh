#!/bin/bash
set -e pipefail

main() {
  export MAKEFLAGS=-j$(getconf _NPROCESSORS_ONLN)
  elixir_vsn="${elixir_vsn:-1.14.0}"

  mkdir -p tmp/cache
  . .github/scripts/app/bootstrap_otp_mac.sh

  elixir_dir="$PWD/tmp/cache/elixir-${elixir_vsn}"
  if [ ! -d "${elixir_dir}" ]; then
    build_elixir $elixir_vsn $elixir_dir
  fi

  export PATH="${elixir_dir}/bin:$PATH"
  echo "checking elixir"
  elixir --version
  echo "elixir ok"
}

# build_elixir $vsn $dest_dir
build_elixir() {
  vsn=$1
  dest_dir=$2
  otp_release=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().')

  cd tmp
  url=https://repo.hex.pm/builds/elixir/v${vsn}-otp-${otp_release}.zip
  curl --fail -LO $url
  mkdir -p $dest_dir
  unzip -q v${vsn}-otp-${otp_release}.zip -d $dest_dir
  cd - > /dev/null
}

main
