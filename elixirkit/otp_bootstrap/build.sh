#!/bin/sh
set -euo pipefail

if [ $# -ne 3 ]; then
  cat <<EOF
Usage:
    build.sh otp_version openssl_version target

Set BUILD_DOCS=1 to build doc chunks.
EOF
  exit 1
fi

otp_version=$1
openssl_version=$2
target=$3
build_docs=${BUILD_DOCS:-}

# Common build flags
export MAKEFLAGS=-j8
cflags="-Os -fno-common -mmacosx-version-min=11.0"

case "$target" in
  macos-aarch64)
    arch="arm64"
    ;;
  macos-x86_64)
    arch="x86_64"
    ;;
  *)
    echo "bad target $target"
    exit 1
esac

build_dir=$PWD/_build
openssl_src_dir=$build_dir/openssl-src-$openssl_version
openssl_rel_dir=$build_dir/openssl-rel-$openssl_version-$target
otp_src_dir=$build_dir/otp-src-$otp_version
otp_rel_dir=$build_dir/otp-rel-$otp_version-openssl-$openssl_version-$target

echo "Building OpenSSL $openssl_version..."

if [ -d $openssl_src_dir ]; then
  echo "$openssl_src_dir already exists"
else
  url=https://github.com/openssl/openssl
  ref=OpenSSL_`echo $openssl_version | tr '.' '_'`
  git clone --depth 1 $url --branch $ref $openssl_src_dir
fi

if [ -d $openssl_rel_dir ]; then
  echo "$openssl_rel_dir already exists"
else
  (
    cd $openssl_src_dir
    ./Configure "darwin64-$arch-cc" --prefix=$openssl_rel_dir $cflags
    make clean
    make
    make install_sw
  )
fi

echo "Building OTP $otp_version..."

if [ -d $otp_src_dir ]; then
  echo "$otp_src_dir already exists"
else
  url=https://github.com/erlang/otp
  ref=OTP-$otp_version
  git clone --depth 1 $url --branch $ref $otp_src_dir
fi

if [ -d $otp_rel_dir ]; then
  echo "$otp_rel_dir already exists"
else
  (
    cd $otp_src_dir
    git clean -dfx
    export ERL_TOP=$PWD
    export ERLC_USE_SERVER=true
    xcrun="xcrun -sdk macosx"
    sysroot=`$xcrun --show-sdk-path`

    ./configure --enable-bootstrap-only

    ./otp_build configure \
      --build=`erts/autoconf/config.guess` \
      --host="$arch-apple-darwin" \
      --with-ssl=$openssl_rel_dir \
      --disable-dynamic-ssl-lib \
      --without-{javac,odbc,wx,observer,debugger,et} \
      erl_xcomp_sysroot=$sysroot \
      CC="$xcrun cc -arch $arch" \
      CFLAGS="$cflags" \
      CXX="$xcrun c++ -arch $arch" \
      CXXFLAGS="$cflags" \
      LD="$xcrun ld" \
      LDFLAGS="-lc++" \
      RANLIB="$xcrun ranlib"

    ./otp_build boot -a
    ./otp_build release -a $otp_rel_dir

    if [ "$build_docs" = "1" ]; then
      make release_docs DOC_TARGETS=chunks RELEASE_ROOT=$otp_rel_dir
    fi

    cd $otp_rel_dir
    ./Install -cross -sasl $PWD
  )
fi

echo "Checking OTP..."

if [[ `uname -m` = "arm64" && "$target" = *"x86_64" ]]; then
  echo "skip"
  exit 0
fi
if [[ `uname -m` = "x86_64" && "$target" = *"aarch64" ]]; then
  echo "skip"
  exit 0
fi

export PATH=$otp_rel_dir/bin:$PATH
erl -noshell -eval 'io:format("root_dir=~p~n", [code:root_dir()]), halt().'
erl -noshell -eval 'io:format("~s", [erlang:system_info(system_version)]), halt().'
erl -noshell -eval 'io:format("~s~n", [erlang:system_info(system_architecture)]), halt().'
erl -noshell -eval 'ok = crypto:start(), io:format("crypto ok~n"), halt().'
