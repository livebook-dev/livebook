#!/bin/sh

# Always generate a random cookie unless set explicitly
if [ -z "${RELEASE_COOKIE}" ]; then
  export RELEASE_COOKIE="$(openssl rand -base64 42)"
else
  export RELEASE_COOKIE
fi

/app/bin/livebook start
