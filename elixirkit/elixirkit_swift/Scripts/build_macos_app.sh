#!/bin/sh
set -euo pipefail

app_name=$ELIXIRKIT_APP_NAME
app_dir=$PWD/.build/${app_name}.app
app_version=`cd "${ELIXIRKIT_PROJECT_DIR}" && MIX_QUIET=1 mix eval "IO.puts Mix.Project.config()[:version]"`
release_name="${ELIXIRKIT_RELEASE_NAME:-}"
build_args="${ELIXIRKIT_BUILD_ARGS:-}"

rm -rf $app_dir
swift build $build_args
target_dir=`swift build --show-bin-path $build_args`
rel_dir=$app_dir/Contents/Resources/rel

mkdir -p $app_dir/Contents/{MacOS,Resources}

if [ -f Info.plist ]; then
  cp Info.plist $app_dir/Contents/Info.plist

  plutil -replace CFBundleVersion -string "${app_version}" $app_dir/Contents/Info.plist
  plutil -replace CFBundleShortVersionString -string "${app_version}" $app_dir/Contents/Info.plist
fi

cp $target_dir/$app_name $app_dir/Contents/MacOS/$app_name

if [ -d Resources ]; then
  for i in Resources/*; do
    cp $i $app_dir/Contents/Resources/
  done
fi

if [ -d Assets.xcassets ]; then
  actool \
    --development-region en --errors --notices --warnings \
    --target-device mac --platform macosx --minimum-deployment-target 11.0 \
    --app-icon AppIcon \
    --compile $app_dir/Contents/Resources \
    --output-partial-info-plist .build/partial-info.plist \
    Assets.xcassets
fi

(
  cd $ELIXIRKIT_PROJECT_DIR
  mix release $release_name --overwrite --path=$rel_dir
)

/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister -f $app_dir
