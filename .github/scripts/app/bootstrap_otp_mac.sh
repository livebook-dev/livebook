#!/bin/bash
set -e pipefail

main() {
  export MAKEFLAGS=-j$(getconf _NPROCESSORS_ONLN)

  wxwidgets_repo="${wxwidgets_repo:-wxWidgets/wxWidgets}"
  wxwidgets_ref="${wxwidgets_ref:-v3.1.7}"
  otp_repo="${otp_repo:-wojtekmach/otp}"
  otp_ref="${otp_ref:-wm-WX_MACOS_NON_GUI_APP}"

  mkdir -p tmp/cache

  openssl_dir=$(brew --prefix openssl@1.1)

  otp_dir="$PWD/tmp/cache/${otp_repo}-${otp_ref}"
  if [ ! -d $otp_dir ]; then
    wxwidgets_dir="$PWD/tmp/cache/${wxwidgets_repo}-${wxwidgets_ref}"
    if [ ! -d $wxwidgets_dir ]; then
      build_wxwidgets $wxwidgets_repo $wxwidgets_ref $wxwidgets_dir
    fi

    export PATH="${wxwidgets_dir}/bin:$PATH"
    echo "checking wx"
    file `which wxrc`
    wx-config --version
    echo "wx ok"
    echo
    build_otp $otp_repo $otp_ref $openssl_dir $otp_dir
  fi

  export PATH="${otp_dir}/bin:$PATH"
  echo "checking otp"
  cd $otp_dir
  ./Install -sasl $PWD
  erl -noshell -eval 'io:format("root_dir=~p~n", [code:root_dir()]), halt().'
  erl -noshell -eval 'io:format("~s", [erlang:system_info(system_version)]), halt().'
  erl -noshell -eval 'io:format("~s~n", [erlang:system_info(system_architecture)]), halt().'
  erl -noshell -eval 'ok = crypto:start(), io:format("crypto ok~n"), halt().'
  erl -noshell -eval '{wx_ref,_,_,_} = wx:new(), io:format("wx ok~n"), halt().'
  cd - > /dev/null
  echo "otp ok"
  echo
}

# build_wxwidgets $repo $ref $dest_dir
build_wxwidgets() {
  repo=$1
  ref=$2
  dest_dir=$3
  src_dir=tmp/$repo-$ref-src

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
    --disable-sys-libs
  make
  make install
  cd - > /dev/null
}


# build_otp $repo $ref $openssl_dir $dest_dir
build_otp() {
  repo=$1
  ref=$2
  openssl_dir=$3
  dest_dir=$4

  src_dir=tmp/otp-$repo-$ref-src
  if [ ! -d $src_dir ]; then
    echo cloning $repo $ref
    git clone --branch $ref --depth 1 --recursive https://github.com/$repo $src_dir
  fi

  export RELEASE_ROOT=$dest_dir

  cd $src_dir
  export ERL_TOP=`pwd`
  export ERLC_USE_SERVER=true
  ./otp_build configure \
    --disable-dynamic-ssl-lib \
    --with-ssl=$openssl_dir \
    --without-odbc

  ./otp_build boot -a
  ./otp_build release -a $RELEASE_ROOT

  if [ -z "$skip_docs" ]; then
    make release_docs DOC_TARGETS=chunks
  fi
  cd - > /dev/null
}

main
