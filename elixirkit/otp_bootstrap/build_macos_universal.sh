#!/bin/sh
set -euo pipefail

if [ $# -ne 2 ]; then
  cat <<EOF
Usage:
    build_macos_universal.sh otp_version openssl_version
EOF
  exit 1
fi

otp_version=$1
openssl_version=$2

echo "\nBuilding macos-universal..."
otp_rel_dir=$PWD/_build/otp-rel-$otp_version-openssl-$openssl_version-macos-universal

if [ -d $otp_rel_dir ]; then
  echo "$otp_rel_dir already exists"
else
  echo "Building macos-aarch64..."
  BUILD_DOCS=1 ./build.sh $otp_version $openssl_version macos-aarch64

  echo "\nBuilding macos-x86_64..."
  ./build.sh $otp_version $openssl_version macos-x86_64

  dir1=$PWD/_build/otp-rel-$otp_version-openssl-$openssl_version-macos-aarch64
  dir2=$PWD/_build/otp-rel-$otp_version-openssl-$openssl_version-macos-x86_64
  cp -R $dir1 $otp_rel_dir
  cd $otp_rel_dir
  for i in `find . -perm +111 -type f -exec sh -c "file {} | grep --silent 'Mach-O'" \; -print`; do
    echo lipo $otp_rel_dir/$i
    lipo $i $dir2/$i -create -output $i.universal
    mv $i.universal $i
  done
  for i in `find . -name "*.a"`; do
    echo lipo $otp_rel_dir/$i
    lipo $i $dir2/$i -create -output $i.universal
    mv $i.universal $i
  done
  cd -
fi

export PATH=$otp_rel_dir/bin:$PATH

echo "checking OTP"
erl -noshell -eval 'io:format("root_dir=~p~n", [code:root_dir()]), halt().'
erl -noshell -eval 'io:format("~s", [erlang:system_info(system_version)]), halt().'
erl -noshell -eval 'io:format("~s~n", [erlang:system_info(system_architecture)]), halt().'
erl -noshell -eval 'ok = crypto:start(), io:format("crypto ok~n"), halt().'
