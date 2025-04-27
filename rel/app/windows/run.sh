#!/bin/sh
set -euo pipefail

. `dirname $0`/build.sh

root="HKEY_CURRENT_USER\\Software\\Classes"

exe="`cmd //c cd`\\bin\\Livebook-$configuration\\Livebook.exe"

echo
echo "!!!!!!"
echo "Setting registry to use $exe"
echo "!!!!!!"
echo

reg add "$root\\.livemd" //d "Livebook.LiveMarkdown" //f
reg add "$root\\Livebook.LiveMarkdown\\DefaultIcon" //d "$exe,1" //f
reg add "$root\\Livebook.LiveMarkdown\\shell\\open\\command" //d "$exe open:%1" //f

reg add "$root\\livebook" //d "URL:Livebook Protocol" //f
reg add "$root\\livebook" //v "URL Protocol" //f
reg add "$root\\Livebook\\DefaultIcon" //d "$exe,1" //f
reg add "$root\\livebook\\shell\\open\\command" //d "$exe open:%1" //f

dotnet run --no-build
