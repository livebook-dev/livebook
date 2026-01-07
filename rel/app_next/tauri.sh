#!/usr/bin/env bash
# Usage: ./tauri.sh [command] [options]
#
# Commands:
#
#   build   see: cargo tauri build --help
#   dev     see: cargo tauri dev --help
#   app     build + open app
set -euo pipefail

main() {
  export MIX_TARGET="app_next"

  root_dir="$(cd "$(dirname "$0")" && pwd)"
  mix_project_dir="${root_dir}/../.."
  app="Livebook"

  case "$(uname -s)" in
    MINGW*|MSYS*|CYGWIN*)
      # When testing in Windows VM and sharing the source code directory with the host,
      # it's better to write build artifacts to regular directories local to the VM.
      # Skip this in CI environments to keep artifacts in standard locations.
      if [ -z "${CI:-}" ]; then
        tmp_dir="/tmp/${app}"
        export MIX_HOME="${tmp_dir}/mix"
        export MIX_DEPS_PATH="${tmp_dir}/mix/deps"
        export MIX_BUILD_PATH="${tmp_dir}/mix/build"
        export HEX_HOME="${tmp_dir}/hex"
        export REBAR_CACHE_DIR="${tmp_dir}/rebar"
        export CARGO_TARGET_DIR="${tmp_dir}/cargo/target"
        export CARGO_HOME="${tmp_dir}/cargo/home"
      fi

      (
        cd "${mix_project_dir}"
        mix.bat local.hex --force --if-missing
        mix.bat local.rebar --force --if-missing
      )
      os=windows
      ;;
    Darwin*)
      os=darwin
      ;;
    Linux*)
      os=linux
      ;;
  esac

  profile="release"
  for arg in "$@"; do
    if [ "$arg" = "--debug" ]; then
      profile="debug"
      break
    fi
  done

  release_root="$root_dir/src-tauri/rel-${os}"

  command="${1:-}"

  config="--config"
  config_json="{\"bundle\":{\"resources\":{\"rel-${os}\":\"rel\"}}}"

  if [ -z "${MIX_ENV:-}" ] && [ "$profile" = "release" ] && [ "$command" != "dev" ]; then
    export MIX_ENV="prod"
  fi

  case "$command" in
    dev)
      cargo tauri "$@"
      ;;
    app)
      shift
      mix_release
      cargo tauri build "$config" "$config_json" "$@"
      open_app "$@"
      ;;
    build)
      shift
      mix_release
      cargo tauri build "$config" "$config_json" "$@"
      ;;
    *)
      cargo tauri "$@"
      ;;
  esac
}

open_app() {
  case "$os" in
    darwin)
      trap 'osascript -e "tell application \"$app\" to quit" >/dev/null 2>&1' INT TERM

      lsregister=/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister
      $lsregister -u /Applications/${app}.app || true

      app_path="$root_dir/src-tauri/target/$profile/bundle/macos/${app}.app"
      open -W --stdout "$(tty)" --stderr "$(tty)" "$app_path" --args "$@"
      ;;
    windows)
      echo "Installing $app..."
      $CARGO_TARGET_DIR/$profile/bundle/nsis/${app}*setup.exe //S

      echo "Running $app..."
      "$LOCALAPPDATA/${app}/${app}.exe" "$@"
      ;;
  esac
}

mix_release() {
  (
    cd "${mix_project_dir}"
    mix deps.get
    mix release app --overwrite --path "$release_root"
  )

  if [ "$os" = "darwin" ] && [ -n "${APPLE_SIGNING_IDENTITY:-}" ]; then
    macos_codesign "$release_root" "$APPLE_SIGNING_IDENTITY"
  fi
}

macos_codesign() {
  local release_path="$1"
  local signing_identity="$2"
  local entitlements_file="$root_dir/src-tauri/App.entitlements"

  local files_to_sign=$(find "$release_path" -perm +111 -type f -exec sh -c 'file "$1" | grep --silent Mach-O && echo "$1"' _ {} \;)
  local file_count=$(echo "$files_to_sign" | wc -l | tr -d ' ')
  echo "Signing $file_count files with identity: $signing_identity"

  # Sign all files with entitlements
  echo "$files_to_sign" | xargs -n 1 -I {} codesign --force --options runtime --entitlements "$entitlements_file" --sign "$signing_identity" --timestamp "{}"
}

main "$@"
