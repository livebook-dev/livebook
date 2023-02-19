#!/bin/sh
set -euo pipefail

app_name=$ELIXIRKIT_APP_NAME
release_name="${ELIXIRKIT_RELEASE_NAME:-}"
app_dir=$PWD/.build/${app_name}.app
build_args="${ELIXIRKIT_BUILD_ARGS:-}"

rm -rf $app_dir
swift build $build_args
target_dir=`swift build --show-bin-path $build_args`
rel_dir=$app_dir/Contents/Resources/rel

mkdir -p $app_dir/Contents/{MacOS,Resources}

if [ -f Info.plist ]; then
  cp Info.plist $app_dir/Contents/Info.plist
fi

cp $target_dir/$app_name $app_dir/Contents/MacOS/$app_name

if [ -d Resources ]; then
  for i in Resources/*; do
    cp $i $app_dir/Contents/Resources/
  done
fi

(
  cd $ELIXIRKIT_PROJECT_DIR
  mix release $release_name --overwrite --path=$rel_dir
)

/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister -f $app_dir
