#!/bin/sh
set -euo pipefail

app_name=$ELIXIRKIT_APP_NAME
app_dir=$PWD/.build/${app_name}.app

identity="${ELIXIRKIT_CODESIGN_IDENTITY:-}"
team_id="${ELIXIRKIT_NOTARY_TEAM_ID:-}"
apple_id="${ELIXIRKIT_NOTARY_APPLE_ID:-}"
password="${ELIXIRKIT_NOTARY_PASSWORD:-}"

if [ -n "$identity" ]; then
  files=`find $app_dir -perm +111 -type f -exec sh -c "file {} | grep --silent Mach-O" \; -print`
  files="$files `find $app_dir -name '*.a'`"
  files="$files $app_dir/Contents/MacOS/$app_name"
  codesign --sign="$identity" --options=runtime --entitlements=App.entitlements --force --timestamp --verbose=2 $files
else
  echo "[warning] skipping codesign. Please set ELIXIRKIT_CODESIGN_IDENTITY environment variable"
fi

dmg_path=$PWD/.build/${app_name}Install.dmg
dmg_dir=$PWD/.build/dmg
rm -rf $dmg_dir
mkdir $dmg_dir
ln -s /Applications $dmg_dir
cp -r $app_dir $dmg_dir/

hdiutil create $dmg_path -ov -volname ${app_name}Install -fs HFS+ -srcfolder $dmg_dir

if [ -n "$team_id" ]; then
  xcrun notarytool submit \
    --team-id "${team_id}" --apple-id "${apple_id}" --password "${password}" \
    --progress \
    --wait \
    $dmg_path
else
  echo "[warning] skipping notarization. Please set ELIXIRKIT_NOTARY_{TEAM_ID,APPLE_ID,PASSWORD} environment variables"
fi
