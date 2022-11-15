set -euo pipefail

if [ $# -ne 3 ]; then
  cat <<EOF
Usage:
    build.sh dir version target
EOF
  exit 1
fi

dir=$1
version=$2
target=$3
src_dir=$dir/openssl-$version
rel_dir=$dir/openssl-$version-$target

case "$target" in
  macos-aarch64)
    openssl_target="darwin64-arm64-cc"
    cc_target="arm64-apple-macos11.0"
    ;;
  macos-x86_64)
    openssl_target="darwin64-x86_64-cc"
    cc_target="x86_64-apple-macos11.0"
    ;;
  *)
    echo bad target $target
    exit 1
esac

if [ ! -d $src_dir ]; then
    cd $dir
    ref=OpenSSL_`echo $version | tr '.' '_'`
    url=https://github.com/openssl/openssl/archive/refs/tags/$ref.tar.gz
    echo downloading $url
    curl --fail -LO $url
    tar xzf $ref.tar.gz
    ls -la
    mv openssl-$ref $src_dir
    rm $ref.tar.gz
fi

if [ ! -d $rel_dir ]; then
    cd $src_dir
    export MAKEFLAGS=-j`nproc`
    ./Configure $openssl_target --prefix=$rel_dir --target=$cc_target
    make clean
    make
    make install_sw
fi
