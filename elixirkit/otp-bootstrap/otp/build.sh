set -euo pipefail

if [ $# -ne 4 ]; then
  cat <<EOF
Usage:
    build.sh dir version target openssl_version
EOF
  exit 1
fi

dir=$1
version=$2
target=$3
openssl_version=$4
src_dir=$dir/otp-$version
rel_dir=$dir/otp-$version-$target
xcomp_conf=`dirname $0`/xcomp/$target.conf
openssl_dir=$dir/openssl-$openssl_version-$target

cd $dir

if [ ! -d $src_dir ]; then
    ref=OTP-$version
    url=https://github.com/erlang/otp/archive/refs/tags/$ref.tar.gz
    echo downloading $url
    curl --fail -LO $url
    tar xzf $ref.tar.gz
    mv otp-$ref $src_dir
    rm $ref.tar.gz
fi

if [[ "$target" = *universal ]]; then
  if [ ! -d $rel_dir ]; then
    target1="${3/universal/aarch64}"
    target2="${3/universal/x86_64}"
    sh $0 $1 $2 $target1 $4
    sh $0 $1 $2 $target2 $4

    cp -r $dir/otp-$version-$target1 $rel_dir
    cd $rel_dir
    for i in `find . -perm +111 -type f -exec sh -c "file {} | grep --silent 'Mach-O'" \; -print`; do
      echo creating `realpath $rel_dir/$i`
      lipo $i $dir/otp-$version-$target2/$i -create -output $i.universal
      mv $i.universal $i
    done
  fi
else
  case "$target" in
    macos-aarch64) otp_target=aarch64-apple-darwin ;;
    macos-x86_64) otp_target=x86_64-apple-darwin ;;
    *) echo bad target $target ; exit 1
  esac

  if [ ! -d $rel_dir ]; then
      cd $src_dir
      export ERL_TOP=$PWD
      export MAKEFLAGS=-j`nproc`
      export ERLC_USE_SERVER=true
      export RELEASE_LIBBEAM=true
      export LIBS=$openssl_dir/lib/libcrypto.a

      ./otp_build configure \
        --xcomp-conf=$xcomp_conf \
        --enable-builtin-zlib \
        --with-ssl=$openssl_dir \
        --disable-dynamic-ssl-lib \
        --without-{javac,odbc,wx,debugger,observer,cdv,et} \
        --enable-static-nifs=$ERL_TOP/lib/crypto/priv/lib/$otp_target/crypto.a,$ERL_TOP/lib/asn1/priv/lib/$otp_target/asn1rt_nif.a

      ./otp_build boot -a
      ./otp_build release -a $rel_dir

      if [[ $target = "macos-aarch64" ]]; then
          make release_docs RELEASE_ROOT=$rel_dir DOC_TARGETS=chunks
      fi
  fi

  cd $src_dir
  mkdir -p $rel_dir/usr/lib

  libtool \
    -static \
    -o $rel_dir/usr/lib/liberl.a \
    erts/emulator/ryu/obj/$otp_target/opt/libryu.a \
    erts/emulator/zlib/obj/$otp_target/opt/libz.a \
    erts/emulator/pcre/obj/$otp_target/opt/libepcre.a \
    erts/lib/internal/$otp_target/lib{erts_internal,ethread}.a \
    bin/$otp_target/libbeam.a \
    $src_dir/lib/crypto/priv/lib/$otp_target/crypto.a \
    $src_dir/lib/asn1/priv/lib/$otp_target/asn1rt_nif.a \
    $openssl_dir/lib/libcrypto.a

  cd $rel_dir
  ./Install -sasl $PWD
  rm -rf misc lib/**/{src,c_src,examples}
fi
