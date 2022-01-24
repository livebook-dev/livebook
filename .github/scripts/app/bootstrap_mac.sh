#!/bin/bash
set -e

main() {
  export MAKEFLAGS=-j$(getconf _NPROCESSORS_ONLN)

  wxwidgets_vsn="3.1.5"
  otp_vsn="24.2"
  elixir_vsn="1.13.2"

  target=$(target)

  mkdir -p tmp

  if [ ! -d "tmp/wxwidgets-${wxwidgets_vsn}-$target" ]; then
    build_wxwidgets $wxwidgets_vsn $target
  fi

  export PATH=$PWD/tmp/wxwidgets-${wxwidgets_vsn}-$target/bin:$PATH
  echo "wx"
  file `which wxrc`
  wx-config --version
  echo

  openssl_dir=$(brew --prefix openssl@1.1)

  if [ ! -d "tmp/otp-${otp_vsn}-$target" ]; then
    build_otp $otp_vsn $target $openssl_dir
  fi

  export PATH=$PWD/tmp/otp-${otp_vsn}-$target/bin:$PATH
  echo "otp"
  file `which erlc`
  erl +V
  erl -noshell -eval 'ok = crypto:start(), io:format("crypto ok~n"), halt().'
  erl -noshell -eval '{wx_ref,_,_,_} = wx:new(), io:format("wx ok~n"), halt().'
  echo

  if [ ! -d "tmp/elixir-${elixir_vsn}" ]; then
    build_elixir $elixir_vsn
  fi

  export PATH=$PWD/tmp/elixir-${elixir_vsn}/bin:$PATH
  echo "elixir"
  elixir --version

  cat << EOF > tmp/bootstrap_env.sh
export PATH=\$PWD/tmp/otp-${otp_vsn}-${target}/bin:\$PATH
export PATH=\$PWD/tmp/elixir-${elixir_vsn}/bin:\$PATH
EOF
}

build_wxwidgets() {
  vsn=$1
  target=$2

  otp_bootstrap_root=$PWD
  cd tmp

  if [ ! -d wxwidgets-$vsn-src ]; then
    url=https://github.com/wxWidgets/wxWidgets/releases/download/v$vsn/wxWidgets-$vsn.tar.bz2
    echo downloading $url
    curl --fail -LO $url
    tar -xf wxWidgets-$vsn.tar.bz2
    mv wxWidgets-$vsn wxwidgets-$vsn-src
  fi

  cd wxwidgets-$vsn-src
  ./configure \
    --disable-shared \
    --prefix=$otp_bootstrap_root/tmp/wxwidgets-$vsn-$target \
    --with-cocoa \
    --with-macosx-version-min=10.15 \
    --with-libjpeg=builtin \
    --with-libtiff=builtin \
    --with-libpng=builtin \
    --with-liblzma=builtin \
    --with-zlib=builtin \
    --with-expat=builtin

  make
  make install
  cd $otp_bootstrap_root
}

build_otp() {
  vsn=$1
  target=$2
  openssl_dir=$3

  otp_bootstrap_root=$PWD
  cd tmp
  curl --fail -LO https://github.com/erlang/otp/releases/download/OTP-${vsn}/otp_src_${vsn}.tar.gz
  tar -xf otp_src_${vsn}.tar.gz

  cd otp_src_${vsn}

  export ERL_TOP=`pwd`
  export RELEASE_ROOT=$otp_bootstrap_root/tmp/otp-$vsn-$target

  ./otp_build configure \
    --disable-dynamic-ssl-lib \
    --with-ssl=$openssl_dir

  ./otp_build boot -a
  ./otp_build release -a $RELEASE_ROOT
  make release_docs DOC_TARGETS=chunks

  cd $RELEASE_ROOT
  ./Install -sasl $PWD
  ./bin/erl -noshell -eval 'io:format("~s", [erlang:system_info(system_version)]), halt().'
  ./bin/erl -noshell -eval 'ok = crypto:start(), halt().'
  ./bin/erl -noshell -eval '{wx_ref,_,_,_} = wx:new(), halt().'
  cd ../..
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
  cd ..
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
