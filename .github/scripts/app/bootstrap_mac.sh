#!/bin/bash
set -e pipefail

main() {
  export MAKEFLAGS=-j$(getconf _NPROCESSORS_ONLN)

  wxwidgets_repo="wxWidgets/wxWidgets"
  wxwidgets_ref="v3.1.7"
  otp_repo="wojtekmach/otp"
  otp_ref="wm-WX_MACOS_NON_GUI_APP"
  elixir_vsn="1.13.4"

  target=$(target)

  mkdir -p tmp

  wxwidgets_dir="$PWD/tmp/wxwidgets-${wxwidgets_ref}-$target"
  if [ ! -d $wxwidgets_dir ]; then
    build_wxwidgets $wxwidgets_repo $wxwidgets_ref $target $wxwidgets_dir
  fi

  export PATH="${wxwidgets_dir}/bin:$PATH"
  echo "checking wx"
  file `which wxrc`
  wx-config --version
  echo

  openssl_dir=$(brew --prefix openssl@1.1)

  otp_dir="$PWD/tmp/otp-${otp_ref}-$target"
  if [ ! -d $otp_dir ]; then
    build_otp $otp_repo $otp_ref $target $openssl_dir $otp_dir
  fi

  export PATH="${otp_dir}/bin:$PATH"
  echo "checking otp"
  file `which erlc`
  erl +V
  erl -noshell -eval 'ok = crypto:start(), io:format("crypto ok~n"), halt().'
  erl -noshell -eval '{wx_ref,_,_,_} = wx:new(), io:format("wx ok~n"), halt().'
  echo

  elixir_dir="$PWD/tmp/elixir-${elixir_vsn}"
  if [ ! -d "${elixir_dir}" ]; then
    build_elixir $elixir_vsn
  fi

  export PATH="${elixir_dir}/bin:$PATH"
  echo "checking elixir"
  elixir --version

  cat << EOF > tmp/bootstrap_env.sh
export PATH="${otp_dir}/bin:\$PATH"
export PATH="${elixir_dir}/bin:\$PATH"
EOF
}

build_wxwidgets() {
  repo=$1
  ref=$2
  target=$3
  dest_dir=$4
  src_dir=tmp/wxwidgets-$ref-src

  if [ ! -d $src_dir ]; then
    echo cloning $repo $ref
    git clone --branch $ref --depth 1 --recursive https://github.com/$repo $src_dir
  fi

  cd $src_dir
  ./configure \
    --disable-shared \
    --prefix=$dest_dir \
    --with-cocoa \
    --with-macosx-version-min=10.15 \
    --with-libjpeg=builtin \
    --with-libtiff=builtin \
    --with-libpng=builtin \
    --with-liblzma=builtin \
    --with-zlib=builtin \
    --with-expat=builtin \
    --with-regex=builtin
  make
  make install
  cd -
}

build_otp() {
  repo=$1
  ref=$2
  target=$3
  openssl_dir=$4
  dest_dir=$5

  src_dir=tmp/otp-$ref-src
  if [ ! -d $src_dir ]; then
    echo cloning $repo $ref
    git clone --branch $ref --depth 1 --recursive https://github.com/$repo $src_dir
  fi

  export RELEASE_ROOT=$dest_dir

  cd $src_dir
  export ERL_TOP=`pwd`
  ./otp_build configure \
    --disable-dynamic-ssl-lib \
    --with-ssl=$openssl_dir

  ./otp_build boot -a
  ./otp_build release -a $RELEASE_ROOT
  make release_docs DOC_TARGETS=chunks
  cd -

  cd $RELEASE_ROOT
  ./Install -sasl $PWD
  ./bin/erl -noshell -eval 'io:format("~s", [erlang:system_info(system_version)]), halt().'
  ./bin/erl -noshell -eval 'ok = crypto:start(), halt().'
  ./bin/erl -noshell -eval '{wx_ref,_,_,_} = wx:new(), halt().'
  cd -
}

build_elixir() {
  vsn=$1
  otp_release=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().')

  cd tmp
  # TODO: On Elixir 1.14, use https://github.com/elixir-lang/elixir/releases/download/v${vsn}/elixir-${vsn}-otp-${otp_release}.zip
  url=https://repo.hex.pm/builds/elixir/v${vsn}-otp-${otp_release}.zip
  curl --fail -LO $url
  mkdir elixir-$vsn
  unzip v${vsn}-otp-${otp_release}.zip -d elixir-$vsn
  cd -
}

target() {
  os=$(uname -s | tr '[:upper:]' '[:lower:]')

  arch=$(uname -m)
  case $arch in
    "arm64") arch="aarch64";;
    *) ;;
  esac

  echo "$arch-$os"
}

main
