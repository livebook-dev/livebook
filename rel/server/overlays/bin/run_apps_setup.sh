#!/bin/sh

cd -P -- "$(dirname -- "$0")"
exec ./livebook eval Livebook.Release.run_apps_setup
